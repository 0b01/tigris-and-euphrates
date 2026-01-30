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
                
                // find all possible tile placements
                for x in 0..H {
                    for y in 0..W {
                        let pos = pos!(x, y);
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

                // move leader
                for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].into_iter() {
                    for pos in state.board.find_empty_leader_space_next_to_red().iter() {
                        moves.push(Action::PlaceLeader { pos, leader });
                    }

                    if let Some(pos) = state.players.get(current_player).get_leader(leader) {
                        moves.push(Action::WithdrawLeader(pos));
                    }
                }

                if state.players.get(current_player).num_catastrophes > 0 {
                    for pos in state.board.find_catastrophe_positions() {
                        moves.push(Action::PlaceCatastrophe(pos));
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
