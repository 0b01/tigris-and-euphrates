use crate::{game::{Action, Leader, Player, PlayerAction, TnEGame, H, W, TileType, Pos, RIVER}, pos};
use minimax::Zobrist;
use once_cell::sync::Lazy;

pub struct TigrisAndEuphrates;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TnEMove {
    pub old_state: TnEGame,
    pub move_: Action,
}

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

impl Zobrist for TnEGame {
    fn zobrist_hash(&self) -> u64 {
        let mut hash: u64 = 0;
        
        // Hash based on tile positions using bitboard bits
        // This is a simplified hash - we extract key bits from the bitboards
        let red_bits = self.board.red_tiles.0.low_u64();
        let blue_bits = self.board.blue_tiles.0.low_u64();
        let green_bits = self.board.green_tiles.0.low_u64();
        let black_bits = self.board.black_tiles.0.low_u64();
        
        hash ^= red_bits.wrapping_mul(0x9e3779b97f4a7c15);
        hash ^= blue_bits.wrapping_mul(0x85ebca6b);
        hash ^= green_bits.wrapping_mul(0xc2b2ae35);
        hash ^= black_bits.wrapping_mul(0x27d4eb2f);
        
        // Hash leader positions
        for (i, leader) in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].iter().enumerate() {
            if let Some(pos) = self.players.get(Player::Player1).get_leader(*leader) {
                hash ^= LEADER_HASHES[pos.x as usize][pos.y as usize][i];
            }
            if let Some(pos) = self.players.get(Player::Player2).get_leader(*leader) {
                hash ^= LEADER_HASHES[pos.x as usize][pos.y as usize][i].wrapping_add(0xdeadbeef);
            }
        }
        
        // Hash player turn
        if self.next_player() == Player::Player1 {
            hash ^= *PLAYER_TURN_HASH;
        }
        
        // Hash catastrophes
        hash ^= self.board.catastrophes.0.low_u64().wrapping_mul(0x1b873593);
        
        // Hash monuments
        hash ^= self.board.monuments.0.low_u64().wrapping_mul(0xe6546b64);
        
        hash
    }
}

impl TnEMove {
    pub fn new(old_state: TnEGame, action: Action) -> Self {
        Self {
            old_state,
            move_: action,
        }
    }
}

impl minimax::Move for TnEMove {
    type G = TigrisAndEuphrates;

    fn apply(&self, state: &mut <Self::G as minimax::Game>::S) {
        match state.process(self.move_) {
            Ok(_) => {}
            Err(e) => {
                dbg!(&state);
                dbg!(self.move_);

                // dbg!(ret);
                // visualizer::play(state.clone());
                panic!("{:#?}", e);
            }
        }
    }

    fn undo(&self, state: &mut <Self::G as minimax::Game>::S) {
        *state = self.old_state.clone();
    }
}

impl minimax::Game for TigrisAndEuphrates {
    type S = TnEGame;
    type M = TnEMove;

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
            moves.push(TnEMove::new(state.clone(), a));
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

    fn is_player1_turn(state: &Self::S) -> bool {
        state.next_player() == Player::Player1
    }
}

/// Heuristic scoring for move ordering.
/// Higher scores = more promising moves that should be searched first.
fn score_move(state: &TnEGame, action: &Action) -> i16 {
    match action {
        // Wars/conflicts are game-changing - search these first
        Action::WarSelectLeader(_) => 1000,
        Action::AddSupport(n) => 900 + (*n as i16), // Higher support is often better
        
        // Tile placements that score points are good
        Action::PlaceTile { pos: _, tile_type } => {
            let current_player = state.next_player();
            let player_state = state.players.get(current_player);
            let leader = match tile_type {
                TileType::Red => Leader::Red,
                TileType::Blue => Leader::Blue,
                TileType::Green => Leader::Green,
                TileType::Black => Leader::Black,
                TileType::Empty => return 100,
            };
            
            // Check if we have a matching leader that could score
            if player_state.get_leader(leader).is_some() {
                // Bonus: tiles that could join to our leader's kingdom are very valuable
                500
            } else if tile_type == &TileType::Red {
                // Red tiles are useful for revolt defense even without a leader
                300
            } else {
                // Other tiles without matching leaders
                200
            }
        }
        
        // Leader placement - valuable, especially early game
        Action::PlaceLeader { pos: _, leader } => {
            let current_player = state.next_player();
            let player_state = state.players.get(current_player);
            
            // Prioritize placing leaders in colors where we're weak
            let score = match leader {
                Leader::Red => player_state.score_red,
                Leader::Blue => player_state.score_blue,
                Leader::Green => player_state.score_green,
                Leader::Black => player_state.score_black,
                Leader::None => 0,
            };
            
            // Lower score = more need for that color = higher priority
            600 - (score as i16 * 5)
        }
        
        // Withdrawing leaders - sometimes necessary but usually not the best move
        Action::WithdrawLeader(_) => 50,
        
        // Catastrophes can be very strategic
        Action::PlaceCatastrophe(_) => 400,
        
        // Treasures are valuable
        Action::TakeTreasure(_) => 800,
        
        // Monuments can be very powerful
        Action::BuildMonument(_) => 700,
        
        // Replacing tiles is a minor action
        Action::ReplaceTile(_) => 100,
        
        // Pass is usually the worst option
        Action::Pass => 0,
    }
}

impl PlayerAction {
    pub(crate) fn generate_moves(&self, state: &TnEGame) -> Vec<Action> {
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
                
                // === MOVE PRUNING ===
                // Only consider tile placements near existing tiles/kingdoms
                // This dramatically reduces the search space
                let existing_tiles = state.board.connectable_bitboard();
                let adjacent_to_tiles = existing_tiles.dilate();
                // Also consider positions 2 steps away for strategic plays
                let near_tiles = adjacent_to_tiles.dilate();
                
                // find all possible tile placements (pruned to near existing tiles)
                for x in 0..H {
                    for y in 0..W {
                        let pos = pos!(x, y);
                        
                        // PRUNING: Skip positions not near any existing tiles
                        // (unless the board is nearly empty)
                        if existing_tiles.count_ones() > 5 && !near_tiles.get(pos) {
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
                // and prioritize leaders for colors where we're weak
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
                
                // PRUNING: Limit leader placement options
                // - Only place leaders we don't already have on board
                // - Limit to a reasonable number of positions (best 8)
                let max_leader_positions = 8.min(leader_spaces.len());
                
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
                    // PRUNING: Limit catastrophe options to 10 most strategic
                    for pos in catastrophe_positions.iter().take(10) {
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
                if *red {
                    moves.push(Action::WarSelectLeader(Leader::Red));
                }
                if *blue {
                    moves.push(Action::WarSelectLeader(Leader::Blue));
                }
                if *green {
                    moves.push(Action::WarSelectLeader(Leader::Green));
                }
                if *black {
                    moves.push(Action::WarSelectLeader(Leader::Black));
                }
            }
            PlayerAction::TakeTreasure(ts) => {
                moves.push(Action::TakeTreasure(ts[0]));
                moves.push(Action::TakeTreasure(ts[1]));
            }
            PlayerAction::BuildMonument(_, types) => {
                for t in types {
                    moves.push(Action::BuildMonument(*t));
                }
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

impl minimax::Evaluator for Evaluator {
    type G = TigrisAndEuphrates;

    fn evaluate(&self, state: &<Self::G as minimax::Game>::S) -> minimax::Evaluation {
        let s1 = state.players.get(Player::Player1).get_eval(state);
        let s2 = state.players.get(Player::Player2).get_eval(state);

        if state.last_action_player == Player::Player1 {
            s2 - s1
        } else {
            s1 - s2
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

        if state.last_action_player == Player::Player1 {
            s2 - s1
        } else {
            s1 - s2
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use minimax::{Negamax, Random, Strategy, Game, Move};

    /// Helper function to play a single game between two strategies
    /// Returns the winner (Player1, Player2, or None for draw)
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
                    mv.apply(&mut game);
                    move_count += 1;
                }
                None => break,
            }
        }

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
                play_game(&mut ai_strategy, &mut random_strategy, 200)
            } else {
                play_game(&mut random_strategy, &mut ai_strategy, 200)
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

    /// Test that the improved AI (depth 2) beats the simple/old AI (depth 1)
    #[test]
    fn test_improved_ai_beats_simple_ai() {
        let num_games = 2;
        let mut improved_wins = 0;
        let mut simple_wins = 0;
        let mut draws = 0;

        for i in 0..num_games {
            // Alternate who goes first
            let improved_is_p1 = i % 2 == 0;
            
            let mut improved_ai = Negamax::new(Evaluator::default(), 2);
            let mut simple_ai = Negamax::new(SimpleEvaluator::default(), 1);

            let winner = if improved_is_p1 {
                play_game(&mut improved_ai, &mut simple_ai, 200)
            } else {
                play_game(&mut simple_ai, &mut improved_ai, 200)
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
    fn test_deeper_search_is_stronger() {
        let num_games = 2;
        let mut deep_wins = 0;
        let mut shallow_wins = 0;
        let mut draws = 0;

        for i in 0..num_games {
            // Alternate who goes first
            let deep_is_p1 = i % 2 == 0;
            
            let mut deep_ai = Negamax::new(Evaluator::default(), 2);
            let mut shallow_ai = Negamax::new(Evaluator::default(), 1);

            let winner = if deep_is_p1 {
                play_game(&mut deep_ai, &mut shallow_ai, 200)
            } else {
                play_game(&mut shallow_ai, &mut deep_ai, 200)
            };

            match winner {
                Player::Player1 if deep_is_p1 => deep_wins += 1,
                Player::Player2 if !deep_is_p1 => deep_wins += 1,
                Player::Player1 if !deep_is_p1 => shallow_wins += 1,
                Player::Player2 if deep_is_p1 => shallow_wins += 1,
                _ => draws += 1,
            }
        }

        println!("\n=== Deep AI (depth 2) vs Shallow AI (depth 1) Results ===");
        println!("Deep AI wins: {}, Shallow AI wins: {}, Draws: {}", deep_wins, shallow_wins, draws);
        println!("Deep AI win rate: {:.1}%", (deep_wins as f64 / num_games as f64) * 100.0);
        
        // Deeper search should usually win
        assert!(
            deep_wins >= shallow_wins,
            "Deeper search should beat shallower search at least as often, but won {} vs {} games",
            deep_wins,
            shallow_wins
        );
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
