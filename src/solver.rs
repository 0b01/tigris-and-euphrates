use crate::{game::{Action, GameState, Leader, Player, PlayerAction, TnEGame, H, W, TileType, Pos, RIVER}, pos};
use once_cell::sync::Lazy;

pub struct TigrisAndEuphrates;

// Zobrist hash constants for leader positions
static LEADER_HASHES: Lazy<[[[u64; 4]; W]; H]> = Lazy::new(|| {
    use std::hash::{Hash, Hasher};
    use std::collections::hash_map::DefaultHasher;
    
    let mut hashes = [[[0u64; 4]; W]; H];
    for x in 0..H {
        for y in 0..W {
            for leader in 0..4 {
                let mut hasher = DefaultHasher::new();
                (x, y, leader, "leader").hash(&mut hasher);
                hashes[x][y][leader] = hasher.finish();
            }
        }
    }
    hashes
});

static PLAYER_TURN_HASH: Lazy<u64> = Lazy::new(|| {
    use std::hash::{Hash, Hasher};
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
    "player_turn".hash(&mut hasher);
    hasher.finish()
});

fn compute_zobrist_hash(state: &TnEGame) -> u64 {
    let mut hash: u64 = 0;
    
    // Hash based on tile positions using bitboard bits
    // This is a simplified hash - we extract key bits from the bitboards
    let red_bits = state.board.red_tiles.0.low_u64();
    let blue_bits = state.board.blue_tiles.0.low_u64();
    let green_bits = state.board.green_tiles.0.low_u64();
    let black_bits = state.board.black_tiles.0.low_u64();
    
    hash ^= red_bits.wrapping_mul(0x9e3779b97f4a7c15);
    hash ^= blue_bits.wrapping_mul(0x85ebca6b);
    hash ^= green_bits.wrapping_mul(0xc2b2ae35);
    hash ^= black_bits.wrapping_mul(0x27d4eb2f);
    
    // Hash leader positions
    for (i, leader) in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].iter().enumerate() {
        if let Some(pos) = state.players.get(Player::Player1).get_leader(*leader) {
            hash ^= LEADER_HASHES[pos.x as usize][pos.y as usize][i];
        }
        if let Some(pos) = state.players.get(Player::Player2).get_leader(*leader) {
            hash ^= LEADER_HASHES[pos.x as usize][pos.y as usize][i].wrapping_add(0xdeadbeef);
        }
    }
    
    // Hash player turn
    if state.next_player() == Player::Player1 {
        hash ^= *PLAYER_TURN_HASH;
    }
    
    // Hash catastrophes
    hash ^= state.board.catastrophes.0.low_u64().wrapping_mul(0x1b873593);
    
    // Hash monuments
    hash ^= state.board.monuments.0.low_u64().wrapping_mul(0xe6546b64);
    
    hash
}

/// Re-export of the Action type as the move type for external usage
pub type TnEMove = Action;

impl minimax::Game for TigrisAndEuphrates {
    type S = TnEGame;
    type M = Action;  // Action is Copy

    fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
        let action = state.next_action();
        let raw_moves = action.generate_moves(state);
        
        // Move ordering: prioritize moves that are likely to be good
        // This improves alpha-beta pruning efficiency
        let mut scored_moves: Vec<(i16, Action)> = raw_moves
            .into_iter()
            .map(|a| (score_move(state, &a), a))
            .collect();
        
        // Sort by score descending (highest scored moves first)
        scored_moves.sort_by(|a, b| b.0.cmp(&a.0));
        
        for (_, a) in scored_moves {
            moves.push(a);
        }
    }

    fn get_winner(state: &Self::S) -> Option<minimax::Winner> {
        if state.state.is_empty() {
            match state.winner() {
                Player::None => Some(minimax::Winner::Draw),
                t if t == state.last_player => Some(minimax::Winner::PlayerJustMoved),
                _ => Some(minimax::Winner::PlayerToMove),
            }
        } else {
            None
        }
    }

    fn apply(state: &mut Self::S, m: Self::M) -> Option<Self::S> {
        // Clone the state, apply the move to the clone, and return the new state
        // The minimax library will use this new state and keep the old one for undo
        let mut new_state = state.clone();
        match new_state.process(m) {
            Ok(_) => {
                // Return the new state - minimax will swap it in
                Some(new_state)
            }
            Err(e) => {
                // This shouldn't happen if generate_moves is correct
                eprintln!("Warning: Invalid move {:?} in state {:?}: {:?}", m, state.next_action(), e);
                // Return None to indicate no change (skip this move)
                None
            }
        }
    }

    fn undo(_state: &mut Self::S, _m: Self::M) {
        // Not used when apply returns Some - the library handles state restoration
    }
    
    fn zobrist_hash(state: &Self::S) -> u64 {
        compute_zobrist_hash(state)
    }

    fn same_player_continues(state: &Self::S) -> bool {
        // In T&E, the same player moves again if:
        // 1. It's still their turn (2 actions per turn)
        // 2. They have pending conflict resolution
        // We check by comparing last_action_player with the player whose turn it is
        state.last_action_player == state.next_player() 
            && state.last_action_player != Player::None
    }
}

/// Heuristic scoring for move ordering.
/// Higher scores = more promising moves that should be searched first.
fn score_move(state: &TnEGame, action: &Action) -> i16 {
    match action {
        // Wars/conflicts are game-changing - search these first
        Action::WarSelectLeader(_) => 1000,
        
        // Support only matters during active conflict (AddSupport state)
        Action::AddSupport(n) => {
            // During conflict, adding support is critical
            if state.state.contains(&GameState::AddSupport) {
                900 + (*n as i16)
            } else {
                // Not in conflict - this shouldn't even be a valid move
                -100
            }
        }
        
        // Tile placements - prioritize tiles that will score points
        Action::PlaceTile { pos, tile_type } => {
            let current_player = state.next_player();
            let opponent = match current_player {
                Player::Player1 => Player::Player2,
                Player::Player2 => Player::Player1,
                Player::None => Player::None,
            };
            let player_state = state.players.get(current_player);
            let opponent_state = state.players.get(opponent);
            let leader = match tile_type {
                TileType::Red => Leader::Red,
                TileType::Blue => Leader::Blue,
                TileType::Green => Leader::Green,
                TileType::Black => Leader::Black,
                TileType::Empty => return 50,
            };
            
            let connectable = state.board.connectable_bitboard();
            let pos_neighbors = pos.neighbors();
            
            // Check if this color is our weakest (min score comes from it)
            let scores = [player_state.score_red, player_state.score_blue,
                          player_state.score_green, player_state.score_black];
            let min_color = *scores.iter().min().unwrap();
            let this_color_score = match tile_type {
                TileType::Red => player_state.score_red,
                TileType::Blue => player_state.score_blue,
                TileType::Green => player_state.score_green,
                TileType::Black => player_state.score_black,
                TileType::Empty => 0,
            };
            let weak_color_bonus = if this_color_score == min_color { 100 } else { 0 };
            
            // Check if opponent would score from this tile
            let opponent_scores = if let Some(opp_pos) = opponent_state.get_leader(leader) {
                let opp_kingdom = state.board.find_kingdom_map_fast(opp_pos, connectable);
                (pos_neighbors & opp_kingdom).count_ones() > 0
            } else {
                false
            };
            
            // Check if we have a matching leader on the board
            if let Some(leader_pos) = player_state.get_leader(leader) {
                let leader_kingdom = state.board.find_kingdom_map_fast(leader_pos, connectable);
                
                // Adjacent to leader's kingdom - this tile will score!
                if (pos_neighbors & leader_kingdom).count_ones() > 0 {
                    // We score! Bonus for weak color, penalty if opponent also scores
                    let base = if opponent_scores { 700 } else { 950 };
                    base + weak_color_bonus
                } else {
                    // Tile far from our leader - lower priority
                    let dist = pos.manhattan_distance(leader_pos);
                    200 - dist.min(10) as i16
                }
            } else if opponent_scores {
                // We don't have leader but opponent does - avoid!
                -100
            } else if *tile_type == TileType::Red {
                // Red tiles are useful for revolt defense
                300
            } else {
                // No matching leader - medium priority
                150
            }
        }
        
        // Leader placement - be careful not to trigger bad revolts
        Action::PlaceLeader { pos, leader } => {
            let current_player = state.next_player();
            let player_state = state.players.get(current_player);
            let opponent = match current_player {
                Player::Player1 => Player::Player2,
                Player::Player2 => Player::Player1,
                Player::None => Player::None,
            };
            let opponent_state = state.players.get(opponent);
            
            // Don't place if already have this leader
            if player_state.get_leader(*leader).is_some() {
                return -200;
            }
            
            // Check revolt defense (nearby red tiles)
            let neighbors = pos.neighbors();
            let nearby_red = (neighbors & state.board.red_tiles).count_ones() as i16;
            
            // Check kingdom for conflicts
            let connectable = state.board.connectable_bitboard();
            let kingdom = state.board.find_kingdom_map_fast(*pos, connectable);
            let kingdom_size = kingdom.count_ones().min(15) as i16;
            
            // Check if there's an opponent leader of SAME color (would trigger revolt)
            let opp_same_leader = opponent_state.get_leader(*leader);
            let causes_revolt = opp_same_leader.map_or(false, |p| kingdom.get(p));
            
            // Also check if ANY opponent leader is in this kingdom (creates tension)
            let opp_in_kingdom = opponent_state.get_leader(Leader::Red).map_or(false, |p| kingdom.get(p))
                || opponent_state.get_leader(Leader::Blue).map_or(false, |p| kingdom.get(p))
                || opponent_state.get_leader(Leader::Green).map_or(false, |p| kingdom.get(p))
                || opponent_state.get_leader(Leader::Black).map_or(false, |p| kingdom.get(p));
            
            // Strong penalty for causing revolt without defense
            let revolt_penalty = if causes_revolt {
                // Revolt! Need good defense (3+ red tiles) to make this worthwhile
                if nearby_red >= 3 { 
                    0  // We have good defense, ok to try
                } else if nearby_red >= 1 { 
                    -150  // Risky
                } else { 
                    -300  // Very bad - we'll likely lose
                }
            } else if opp_in_kingdom {
                // Opponent is here but no immediate revolt - still risky for future wars
                -50
            } else {
                0
            };
            
            // Prefer kingdoms without opponent presence
            // Base priority + defense + kingdom size + revolt consideration
            350 + (nearby_red * 15) + (kingdom_size * 2) + revolt_penalty
        }
        
        // Withdrawing leaders is usually bad - low priority
        // Only do it if there's a very good reason (escaping a losing war)
        Action::WithdrawLeader(_) => -100,
        
        // Strategic actions
        Action::PlaceCatastrophe(_) => 200,  // Lower priority - save for emergencies
        Action::TakeTreasure(_) => 1000,  // Very valuable - wildcards!
        Action::BuildMonument(_) => 900,  // Great for ongoing points
        Action::DeclineMonument => 30,
        Action::ReplaceTile(_) => 100,
        Action::Pass => 5,
    }
}

impl PlayerAction {
    pub fn generate_moves(&self, state: &TnEGame) -> Vec<Action> {
        let current_player = state.next_player();
        let mut moves = vec![];

        match self {
            PlayerAction::AddSupport(tile_type) => {
                let max = state.players.get(current_player).get_hand(*tile_type);
                for n in 0..=max {
                    moves.push(Action::AddSupport(n));
                }
            }
            PlayerAction::Normal => {
                // Precompute kingdom count for checking tile placements
                let kingdom_count = state.board.nearby_kingdom_count();
                
                // Only consider tile placements adjacent to existing tiles
                // This dramatically reduces the search space
                let existing_tiles = state.board.connectable_bitboard();
                let adjacent_to_tiles = existing_tiles.dilate();
                
                // find all possible tile placements (pruned to adjacent to existing tiles)
                for x in 0..H {
                    for y in 0..W {
                        let pos = pos!(x, y);
                        
                        // Skip positions not adjacent to any existing tiles
                        // (unless the board is nearly empty)
                        if existing_tiles.count_ones() > 3 && !adjacent_to_tiles.get(pos) {
                            continue;
                        }
                        
                        let river = RIVER.get(pos);
                        for tile_type in [TileType::Black, TileType::Red, TileType::Green, TileType::Blue].into_iter() {
                            if state.players.get(current_player).get_hand(tile_type) > 0
                            && state.board.can_place_tile(pos)
                            && (river == (tile_type == TileType::Blue))
                            && kingdom_count[x][y] <= 2 {
                                moves.push(Action::PlaceTile {
                                    pos,
                                    tile_type,
                                });
                            }
                        }
                    }
                }

                // move leader - only place leaders we don't already have on board
                let player_state = state.players.get(current_player);
                let scores = [
                    (Leader::Red, player_state.score_red),
                    (Leader::Blue, player_state.score_blue),
                    (Leader::Green, player_state.score_green),
                    (Leader::Black, player_state.score_black),
                ];
                
                // Sort leaders by score (weakest first) for better pruning
                let mut sorted_leaders: Vec<_> = scores.iter().collect();
                sorted_leaders.sort_by_key(|(_, score)| *score);
                
                let leader_spaces_bitboard = state.board.find_empty_leader_space_next_to_red();
                let leader_spaces: Vec<Pos> = leader_spaces_bitboard.iter().collect();
                
                // PRUNING: Limit leader placement options aggressively
                // - Only place leaders we don't already have on board
                // - Limit to 5 positions for faster search
                let max_leader_positions = 5.min(leader_spaces.len());
                
                for (leader, _) in sorted_leaders {
                    // Skip if we already have this leader placed
                    if player_state.get_leader(*leader).is_some() {
                        // But allow withdrawal
                        if let Some(pos) = player_state.get_leader(*leader) {
                            moves.push(Action::WithdrawLeader(pos));
                        }
                        continue;
                    }
                    
                    // Only consider limited positions for new leader placement
                    for pos in leader_spaces.iter().take(max_leader_positions) {
                        moves.push(Action::PlaceLeader { pos: *pos, leader: *leader });
                    }
                }

                // Catastrophe placements - these are rare but strategic
                if state.players.get(current_player).num_catastrophes > 0 {
                    let catastrophe_positions = state.board.find_catastrophe_positions();
                    // PRUNING: Limit catastrophe options to 5 most strategic
                    for pos in catastrophe_positions.iter().take(5) {
                        moves.push(Action::PlaceCatastrophe(*pos));
                    }
                }

                moves.push(Action::Pass);
            }
            PlayerAction::SelectLeader {
                red,
                blue,
                green,
                black,
            } => {
                // Get the external conflict to check if leaders are still connected
                let mut any_valid = false;
                if let Some(conflicts) = &state.external_conflict {
                    for conflict in &conflicts.conflicts {
                        // Only generate move if leaders are still connected
                        if state.board.path_find(conflict.attacker_pos, conflict.defender_pos) {
                            let leader = conflict.conflict_leader;
                            match leader {
                                Leader::Red if *red => {
                                    moves.push(Action::WarSelectLeader(Leader::Red));
                                    any_valid = true;
                                }
                                Leader::Blue if *blue => {
                                    moves.push(Action::WarSelectLeader(Leader::Blue));
                                    any_valid = true;
                                }
                                Leader::Green if *green => {
                                    moves.push(Action::WarSelectLeader(Leader::Green));
                                    any_valid = true;
                                }
                                Leader::Black if *black => {
                                    moves.push(Action::WarSelectLeader(Leader::Black));
                                    any_valid = true;
                                }
                                _ => {}
                            }
                        }
                    }
                }
                
                // If no valid moves due to disconnection, generate all options 
                // and let the game handle the invalid ones (they get skipped)
                if !any_valid {
                    if *red { moves.push(Action::WarSelectLeader(Leader::Red)); }
                    if *blue { moves.push(Action::WarSelectLeader(Leader::Blue)); }
                    if *green { moves.push(Action::WarSelectLeader(Leader::Green)); }
                    if *black { moves.push(Action::WarSelectLeader(Leader::Black)); }
                }
            }
            PlayerAction::TakeTreasure(ts) => {
                // Must take border treasures first according to the rules
                let border_treasures: Vec<_> = ts.iter()
                    .filter(|t| crate::game::SPECIAL_BORDER_POSITIONS.get(**t))
                    .collect();
                
                if !border_treasures.is_empty() {
                    // Only generate moves for border treasures
                    for t in border_treasures {
                        moves.push(Action::TakeTreasure(*t));
                    }
                } else {
                    // No border treasures, can take any
                    for t in ts.iter() {
                        moves.push(Action::TakeTreasure(*t));
                    }
                }
            }
            PlayerAction::BuildMonument(_, types) => {
                for t in types {
                    moves.push(Action::BuildMonument(*t));
                }
                // Player can also decline to build a monument
                moves.push(Action::DeclineMonument);
            }
        }

        moves
    }
}

pub struct Evaluator;

impl Default for Evaluator {
    fn default() -> Self {
        Self {}
    }
}

impl Evaluator {
    /// Evaluate a player's position - this is the experimental version
    pub fn eval_player(player: &crate::game::PlayerState, state: &TnEGame) -> i16 {
        let mut s: i16 = 0;

        // Min score weighted 100x - this is THE scoring metric
        let min_score = player.calculate_score() as i16;
        s += min_score * 100;
        
        // Raw score sum - try higher weight to encourage aggressive point collection
        s += player.score_sum() as i16 * 5;  // Was 3

        // Treasure bonus - slightly lower, they're already counted in min_score
        s += player.score_treasure as i16 * 80;  // Was 100
        
        // Balance bonus - 20 per nonzero color
        let scores = [player.score_red, player.score_blue, player.score_green, player.score_black];
        let nonzero_colors = scores.iter().filter(|&&x| x > 0).count() as i16;
        s += nonzero_colors * 20;

        // Leader presence - higher value, leaders are essential for scoring
        let leaders_on_board = [
            player.placed_red_leader, player.placed_blue_leader,
            player.placed_green_leader, player.placed_black_leader
        ].iter().filter(|x| x.is_some()).count() as i16;
        s += leaders_on_board * 15;  // Was 10

        // Monument control - 40 per controlled color
        let connectable = state.board.connectable_bitboard();
        for monument in &state.monuments {
            let leaders = monument.monument_type.unpack();
            let monument_pos = monument.pos_top_left;
            let monument_kingdom = state.board.find_kingdom_map_fast(monument_pos, connectable);
            
            for &leader in &leaders {
                if let Some(leader_pos) = player.get_leader(leader) {
                    if monument_kingdom.get(leader_pos) {
                        s += 40;
                    }
                }
            }
        }
        
        // Hand tiles bonus - 2 per tile
        let hand_size = (player.hand_red + player.hand_blue + player.hand_green + player.hand_black) as i16;
        s += hand_size * 2;
        
        // Extra red tile bonus (useful for revolt defense) - CONFIRMED
        s += player.hand_red as i16 * 3;

        s
    }
}

impl minimax::Evaluator for Evaluator {
    type G = TigrisAndEuphrates;

    fn evaluate(&self, state: &<Self::G as minimax::Game>::S) -> minimax::Evaluation {
        let s1 = Self::eval_player(state.players.get(Player::Player1), state);
        let s2 = Self::eval_player(state.players.get(Player::Player2), state);

        // Standard negamax expects: positive = good for player to move
        let next = state.next_player();
        if next == Player::Player1 {
            s1 - s2
        } else {
            s2 - s1
        }
    }
}

/// Baseline evaluator - the current "best" version to compare against
/// Copy the current get_eval logic here before making changes
pub struct BaselineEvaluator;

impl Default for BaselineEvaluator {
    fn default() -> Self {
        Self {}
    }
}

impl BaselineEvaluator {
    fn eval(player_state: &crate::game::PlayerState, state: &TnEGame) -> i16 {
        let mut s: i16 = 0;

        // Min score weighted 100x
        let min_score = player_state.calculate_score() as i16;
        s += min_score * 100;
        
        // Raw score sum * 3 (was 5) - CONFIRMED
        s += player_state.score_sum() as i16 * 3;

        // Treasure bonus 100
        s += player_state.score_treasure as i16 * 100;
        
        // Balance bonus - 20 per nonzero color
        let scores = [player_state.score_red, player_state.score_blue, 
                      player_state.score_green, player_state.score_black];
        let nonzero_colors = scores.iter().filter(|&&x| x > 0).count() as i16;
        s += nonzero_colors * 20;

        // Leader presence - 10 per leader
        let leaders_on_board = [
            player_state.placed_red_leader, player_state.placed_blue_leader,
            player_state.placed_green_leader, player_state.placed_black_leader
        ].iter().filter(|x| x.is_some()).count() as i16;
        s += leaders_on_board * 10;

        // Monument control - 40 per controlled color
        let connectable = state.board.connectable_bitboard();
        for monument in &state.monuments {
            let leaders = monument.monument_type.unpack();
            let monument_pos = monument.pos_top_left;
            let monument_kingdom = state.board.find_kingdom_map_fast(monument_pos, connectable);
            
            for &leader in &leaders {
                if let Some(leader_pos) = player_state.get_leader(leader) {
                    if monument_kingdom.get(leader_pos) {
                        s += 40;
                    }
                }
            }
        }
        
        // Hand tiles bonus - more tiles = more options (CONFIRMED)
        let hand_size = (player_state.hand_red + player_state.hand_blue + 
                         player_state.hand_green + player_state.hand_black) as i16;
        s += hand_size * 2;
        
        // Extra red tile bonus - useful for revolt defense (CONFIRMED)
        s += player_state.hand_red as i16 * 3;

        s
    }
}

impl minimax::Evaluator for BaselineEvaluator {
    type G = TigrisAndEuphrates;

    fn evaluate(&self, state: &<Self::G as minimax::Game>::S) -> minimax::Evaluation {
        let s1 = Self::eval(state.players.get(Player::Player1), state);
        let s2 = Self::eval(state.players.get(Player::Player2), state);

        let next = state.next_player();
        if next == Player::Player1 {
            s1 - s2
        } else {
            s2 - s1
        }
    }
}

/// A simple evaluator that only considers the raw score (for testing the old AI)
pub struct SimpleEvaluator;

impl Default for SimpleEvaluator {
    fn default() -> Self {
        Self {}
    }
}

impl SimpleEvaluator {
    /// Simple evaluation - just uses raw scores like the old AI
    fn simple_eval(player_state: &crate::game::PlayerState) -> i16 {
        let mut s: i16 = 0;
        // Old evaluation: 20 points for final score
        s += player_state.calculate_score() as i16 * 20;
        // 1 point for each raw score point
        s += player_state.score_sum() as i16;
        s
    }
}

impl minimax::Evaluator for SimpleEvaluator {
    type G = TigrisAndEuphrates;

    fn evaluate(&self, state: &<Self::G as minimax::Game>::S) -> minimax::Evaluation {
        let s1 = Self::simple_eval(state.players.get(Player::Player1));
        let s2 = Self::simple_eval(state.players.get(Player::Player2));

        // Standard negamax: positive = good for player to move
        let next = state.next_player();
        if next == Player::Player1 {
            s1 - s2
        } else {
            s2 - s1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use minimax::{Negamax, Random, Strategy, Game};

    /// Helper function to play a single game between two strategies
    /// Returns the winner based on score after max_moves (or game end)
    fn play_game<S1: Strategy<TigrisAndEuphrates>, S2: Strategy<TigrisAndEuphrates>>(
        p1_strategy: &mut S1,
        p2_strategy: &mut S2,
        max_moves: usize,
    ) -> Player {
        let mut game = TnEGame::new();
        let mut move_count = 0;

        while TigrisAndEuphrates::get_winner(&game).is_none() && move_count < max_moves {
            let curr_player = game.next_player();
            let m = match curr_player {
                Player::Player1 => p1_strategy.choose_move(&game),
                Player::Player2 => p2_strategy.choose_move(&game),
                _ => None,
            };

            match m {
                Some(mv) => {
                    // apply returns a new state, we need to use it
                    if let Some(new_state) = TigrisAndEuphrates::apply(&mut game, mv) {
                        game = new_state;
                    }
                    move_count += 1;
                }
                None => break,
            }
        }

        // Debug scores
        let p1 = game.players.get(Player::Player1);
        let p2 = game.players.get(Player::Player2);
        let p1_min = p1.calculate_score();
        let p2_min = p2.calculate_score();
        println!("Game ended after {} moves:", move_count);
        println!("  P1: min={} [R:{} Bk:{} Bl:{} G:{} T:{}]", 
            p1_min, p1.score_red, p1.score_black, p1.score_blue, p1.score_green, p1.score_treasure);
        println!("  P2: min={} [R:{} Bk:{} Bl:{} G:{} T:{}]",
            p2_min, p2.score_red, p2.score_black, p2.score_blue, p2.score_green, p2.score_treasure);

        // Use score comparison (works even if game didn't end naturally)
        game.winner()
    }

    /// Test that the improved AI beats a random player consistently
    #[test]
    fn test_improved_ai_beats_random() {
        let num_games = 4;
        let mut ai_wins = 0;
        let mut random_wins = 0;
        let mut draws = 0;

        for i in 0..num_games {
            // Alternate who goes first
            let ai_is_p1 = i % 2 == 0;
            
            // Use depth 2 for faster tests
            let mut ai_strategy = Negamax::new(Evaluator::default(), 2);
            let mut random_strategy = Random::<TigrisAndEuphrates>::new();

            let winner = if ai_is_p1 {
                play_game(&mut ai_strategy, &mut random_strategy, 50)
            } else {
                play_game(&mut random_strategy, &mut ai_strategy, 50)
            };

            match winner {
                Player::Player1 if ai_is_p1 => ai_wins += 1,
                Player::Player2 if !ai_is_p1 => ai_wins += 1,
                Player::Player1 if !ai_is_p1 => random_wins += 1,
                Player::Player2 if ai_is_p1 => random_wins += 1,
                _ => draws += 1,
            }
        }

        println!("\n=== AI vs Random Results ===");
        println!("AI wins: {}, Random wins: {}, Draws: {}", ai_wins, random_wins, draws);
        println!("AI win rate: {:.1}%", (ai_wins as f64 / num_games as f64) * 100.0);
        
        // The AI should win at least 50% of games against random
        assert!(
            ai_wins >= num_games / 2,
            "AI should beat random at least 50% of the time, but only won {} out of {} games",
            ai_wins,
            num_games
        );
    }

    /// Test that the improved AI (depth 2) beats the simple/old AI (depth 2)
    #[test]
    fn test_improved_ai_beats_simple_ai() {
        let num_games = 20;  // More games for statistical significance
        let mut improved_wins = 0;
        let mut simple_wins = 0;
        let mut draws = 0;

        for i in 0..num_games {
            // Alternate who goes first
            let improved_is_p1 = i % 2 == 0;
            
            // Same depth for fair comparison
            let mut improved_ai = Negamax::new(Evaluator::default(), 2);
            let mut simple_ai = Negamax::new(SimpleEvaluator::default(), 2);

            let winner = if improved_is_p1 {
                play_game(&mut improved_ai, &mut simple_ai, 100)
            } else {
                play_game(&mut simple_ai, &mut improved_ai, 100)
            };

            match winner {
                Player::Player1 if improved_is_p1 => improved_wins += 1,
                Player::Player2 if !improved_is_p1 => improved_wins += 1,
                Player::Player1 if !improved_is_p1 => simple_wins += 1,
                Player::Player2 if improved_is_p1 => simple_wins += 1,
                _ => draws += 1,
            }
        }

        println!("\n=== Improved AI vs Simple AI Results ===");
        println!("Improved AI wins: {}, Simple AI wins: {}, Draws: {}", improved_wins, simple_wins, draws);
        println!("Improved AI win rate: {:.1}%", (improved_wins as f64 / num_games as f64) * 100.0);
        
        // The improved AI should win at least half the games against the simple AI
        assert!(
            improved_wins >= simple_wins,
            "Improved AI should beat simple AI at least as often, but won {} vs {} games",
            improved_wins,
            simple_wins
        );
    }

    /// Test that deeper search produces better results
    #[test]
    #[ignore]
    fn test_depth_scaling() {
        println!("\n=== Depth Scaling Test ===");
        
        // Test depth 2 vs 1
        let (d2_wins, d1_wins) = run_depth_match(2, 1, 20);
        println!("Depth 2 vs 1: {} - {} ({:.0}%)", d2_wins, d1_wins, 
            d2_wins as f64 / (d2_wins + d1_wins) as f64 * 100.0);
        
        // Test depth 3 vs 2
        let (d3_wins, d2_wins2) = run_depth_match(3, 2, 10);
        println!("Depth 3 vs 2: {} - {} ({:.0}%)", d3_wins, d2_wins2,
            d3_wins as f64 / (d3_wins + d2_wins2) as f64 * 100.0);
        
        // Test depth 4 vs 3
        let (d4_wins, d3_wins2) = run_depth_match(4, 3, 6);
        println!("Depth 4 vs 3: {} - {} ({:.0}%)", d4_wins, d3_wins2,
            d4_wins as f64 / (d4_wins + d3_wins2) as f64 * 100.0);
    }
    
    fn run_depth_match(deep: u8, shallow: u8, num_games: usize) -> (usize, usize) {
        let mut deep_wins = 0;
        let mut shallow_wins = 0;

        for i in 0..num_games {
            let deep_is_p1 = i % 2 == 0;
            
            let mut deep_ai = Negamax::new(Evaluator::default(), deep);
            let mut shallow_ai = Negamax::new(Evaluator::default(), shallow);

            let winner = if deep_is_p1 {
                play_game(&mut deep_ai, &mut shallow_ai, 80)
            } else {
                play_game(&mut shallow_ai, &mut deep_ai, 80)
            };

            match winner {
                Player::Player1 if deep_is_p1 => deep_wins += 1,
                Player::Player2 if !deep_is_p1 => deep_wins += 1,
                Player::Player1 if !deep_is_p1 => shallow_wins += 1,
                Player::Player2 if deep_is_p1 => shallow_wins += 1,
                _ => {}
            }
        }
        (deep_wins, shallow_wins)
    }

    /// A/B test: Compare current Evaluator (experimental) vs BaselineEvaluator (known good)
    /// Run this after making changes to get_eval() to see if they help
    #[test]
    fn test_experimental_vs_baseline() {
        let num_games = 40;  // More games in single run for speed
        let mut experimental_wins = 0;
        let mut baseline_wins = 0;
        let mut draws = 0;

        for i in 0..num_games {
            // Alternate who goes first
            let experimental_is_p1 = i % 2 == 0;
            
            // Same depth for fair comparison
            let mut experimental_ai = Negamax::new(Evaluator::default(), 2);
            let mut baseline_ai = Negamax::new(BaselineEvaluator::default(), 2);

            let winner = if experimental_is_p1 {
                play_game(&mut experimental_ai, &mut baseline_ai, 100)
            } else {
                play_game(&mut baseline_ai, &mut experimental_ai, 100)
            };

            match winner {
                Player::Player1 if experimental_is_p1 => experimental_wins += 1,
                Player::Player2 if !experimental_is_p1 => experimental_wins += 1,
                Player::Player1 if !experimental_is_p1 => baseline_wins += 1,
                Player::Player2 if experimental_is_p1 => baseline_wins += 1,
                _ => draws += 1,
            }
        }

        println!("\n=== Experimental vs Baseline Results ===");
        println!("Experimental wins: {}, Baseline wins: {}, Draws: {}", 
            experimental_wins, baseline_wins, draws);
        let win_rate = experimental_wins as f64 / num_games as f64 * 100.0;
        println!("Experimental win rate: {:.1}%", win_rate);
        
        if experimental_wins > baseline_wins {
            println!("✓ IMPROVEMENT: Experimental is better! Keep the change.");
        } else if experimental_wins < baseline_wins {
            println!("✗ REGRESSION: Baseline is better. Revert the change.");
        } else {
            println!("≈ NO CHANGE: About the same. Consider if change adds value.");
        }
    }

    // ==================== ELO RATING SYSTEM ====================
    
    /// ELO rating calculation constants
    const INITIAL_ELO: f64 = 1500.0;
    const K_FACTOR: f64 = 32.0;
    
    /// Calculate expected score based on ELO ratings
    fn expected_score(player_elo: f64, opponent_elo: f64) -> f64 {
        1.0 / (1.0 + 10.0_f64.powf((opponent_elo - player_elo) / 400.0))
    }
    
    /// Update ELO rating after a game
    /// actual_score: 1.0 for win, 0.5 for draw, 0.0 for loss
    fn update_elo(current_elo: f64, expected: f64, actual: f64) -> f64 {
        current_elo + K_FACTOR * (actual - expected)
    }
    
    /// Run a tournament between AI versions and calculate ELO ratings
    /// This gives an objective measure of AI strength
    /// 
    /// Run with: cargo test --release test_elo_tournament -- --nocapture --ignored
    #[test]
    #[ignore] // This is a benchmark test - run manually for AI strength analysis
    fn test_elo_tournament() {
        println!("\n");
        println!("╔══════════════════════════════════════════════════════════════╗");
        println!("║           TIGRIS & EUPHRATES AI ELO TOURNAMENT               ║");
        println!("╠══════════════════════════════════════════════════════════════╣");
        println!("║  Measuring AI strength using the ELO rating system           ║");
        println!("║  (Same system used on Board Game Arena for T&E rankings)     ║");
        println!("╚══════════════════════════════════════════════════════════════╝");
        println!();
        
        // AI Players in the tournament
        // Index: 0=Random, 1=SimpleD1, 2=SimpleD2, 3=ImprovedD1, 4=ImprovedD2
        let mut elos = [INITIAL_ELO; 5];
        let names = [
            "Random Player",
            "Simple AI (d1)",
            "Simple AI (d2)", 
            "Improved AI (d1)",
            "Improved AI (d2)",
        ];
        
        let games_per_matchup = 4; // Play 4 games per matchup for better statistics
        let mut results = [[0i32; 5]; 5]; // wins[i][j] = wins of player i against player j
        
        println!("Running tournament matches ({} games per matchup)...", games_per_matchup);
        
        // Round-robin tournament
        for i in 0..5 {
            for j in (i+1)..5 {
                for game_num in 0..games_per_matchup {
                    let i_is_p1 = game_num % 2 == 0;
                    
                    // Create strategies for this matchup
                    let winner = match (i, j) {
                        // Random vs others
                        (0, 1) => {
                            let mut p1 = Random::<TigrisAndEuphrates>::new();
                            let mut p2 = Negamax::new(SimpleEvaluator::default(), 1);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        (0, 2) => {
                            let mut p1 = Random::<TigrisAndEuphrates>::new();
                            let mut p2 = Negamax::new(SimpleEvaluator::default(), 2);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        (0, 3) => {
                            let mut p1 = Random::<TigrisAndEuphrates>::new();
                            let mut p2 = Negamax::new(Evaluator::default(), 1);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        (0, 4) => {
                            let mut p1 = Random::<TigrisAndEuphrates>::new();
                            let mut p2 = Negamax::new(Evaluator::default(), 2);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        // Simple AI matchups
                        (1, 2) => {
                            let mut p1 = Negamax::new(SimpleEvaluator::default(), 1);
                            let mut p2 = Negamax::new(SimpleEvaluator::default(), 2);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        (1, 3) => {
                            let mut p1 = Negamax::new(SimpleEvaluator::default(), 1);
                            let mut p2 = Negamax::new(Evaluator::default(), 1);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        (1, 4) => {
                            let mut p1 = Negamax::new(SimpleEvaluator::default(), 1);
                            let mut p2 = Negamax::new(Evaluator::default(), 2);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        (2, 3) => {
                            let mut p1 = Negamax::new(SimpleEvaluator::default(), 2);
                            let mut p2 = Negamax::new(Evaluator::default(), 1);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        (2, 4) => {
                            let mut p1 = Negamax::new(SimpleEvaluator::default(), 2);
                            let mut p2 = Negamax::new(Evaluator::default(), 2);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        // Improved AI matchups
                        (3, 4) => {
                            let mut p1 = Negamax::new(Evaluator::default(), 1);
                            let mut p2 = Negamax::new(Evaluator::default(), 2);
                            if i_is_p1 { play_game(&mut p1, &mut p2, 200) }
                            else { play_game(&mut p2, &mut p1, 200) }
                        }
                        _ => Player::None,
                    };
                    
                    // Determine who won relative to i and j
                    let (i_score, j_score) = match winner {
                        Player::Player1 if i_is_p1 => (1.0, 0.0),
                        Player::Player2 if i_is_p1 => (0.0, 1.0),
                        Player::Player1 if !i_is_p1 => (0.0, 1.0),
                        Player::Player2 if !i_is_p1 => (1.0, 0.0),
                        _ => (0.5, 0.5), // Draw
                    };
                    
                    // Update results
                    if i_score == 1.0 { results[i][j] += 1; }
                    if j_score == 1.0 { results[j][i] += 1; }
                    
                    // Update ELO ratings
                    let expected_i = expected_score(elos[i], elos[j]);
                    let expected_j = expected_score(elos[j], elos[i]);
                    elos[i] = update_elo(elos[i], expected_i, i_score);
                    elos[j] = update_elo(elos[j], expected_j, j_score);
                }
            }
        }
        
        // Print results
        println!();
        println!("╔══════════════════════════════════════════════════════════════╗");
        println!("║                    TOURNAMENT RESULTS                        ║");
        println!("╠══════════════════════════════════════════════════════════════╣");
        
        // Sort by ELO
        let mut ranked: Vec<(usize, f64)> = elos.iter().enumerate().map(|(i, &e)| (i, e)).collect();
        ranked.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        
        println!("║  Rank │ Player                    │ ELO Rating │ vs Random  ║");
        println!("╠══════════════════════════════════════════════════════════════╣");
        
        for (rank, (idx, elo)) in ranked.iter().enumerate() {
            let vs_random = if *idx == 0 { 
                "N/A".to_string() 
            } else { 
                format!("{}-{}", results[*idx][0], results[0][*idx])
            };
            println!("║  {:>4} │ {:25} │ {:>10.0} │ {:>10} ║", 
                     rank + 1, names[*idx], elo, vs_random);
        }
        
        println!("╚══════════════════════════════════════════════════════════════╝");
        println!();
        
        // Calculate ELO difference between best improved AI and random
        let improved_d2_elo = elos[4];
        let random_elo = elos[0];
        let elo_diff = improved_d2_elo - random_elo;
        
        println!("📊 ELO Analysis:");
        println!("   • Improved AI (d2) ELO: {:.0}", improved_d2_elo);
        println!("   • Random Player ELO: {:.0}", random_elo);
        println!("   • ELO Difference: {:.0} points", elo_diff);
        println!();
        
        // ELO interpretation
        println!("📈 What does this mean?");
        println!("   • +100 ELO ≈ 64% expected win rate");
        println!("   • +200 ELO ≈ 76% expected win rate");
        println!("   • +400 ELO ≈ 91% expected win rate");
        println!();
        
        let win_rate = expected_score(improved_d2_elo, random_elo) * 100.0;
        println!("   🎯 Our AI's expected win rate vs Random: {:.1}%", win_rate);
        println!();
        
        // Context for Board Game Arena
        println!("🌐 Board Game Arena Context:");
        println!("   • BGA uses ELO for T&E rankings");
        println!("   • Top 3 players have ELO > 300 above average");
        println!("   • Our AI shows {:.0} ELO above random baseline", elo_diff);
        println!();
        
        // Note about statistical significance
        println!("⚠️  Note: With only {} games per matchup, results can vary.", games_per_matchup);
        println!("   Increase games_per_matchup for more statistically significant rankings.");
        
        // This is a benchmark test - no assertions, just reporting
        // The ELO results speak for themselves
    }
}
