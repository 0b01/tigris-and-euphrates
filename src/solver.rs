use crate::{game::{Action, Leader, Movement, Player, PlayerAction, TnEGame, H, W, TileType, Pos, RIVER}, visualizer, pos};
use minimax::Zobrist;
pub struct TigrisAndEuphrates;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TnEMove {
    pub old_state: TnEGame,
    pub move_: Action,
}

impl Zobrist for TnEGame {
    fn zobrist_hash(&self) -> u64 {
        0
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
        for a in action.generate_moves(state) {
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
                // find all possible tile placements
                for x in 0..H {
                    for y in 0..W {
                        let pos = pos!(x, y);
                        let river = RIVER.get(pos);
                        for tile_type in [TileType::Black, TileType::Red, TileType::Green, TileType::Blue].into_iter() {
                            if state.players.get(current_player).get_hand(tile_type) > 0
                            && state.board.can_place_tile(pos)
                            && (river == (tile_type == TileType::Blue)) {
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
