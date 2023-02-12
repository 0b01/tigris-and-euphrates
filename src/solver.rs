use minimax::Zobrist;

use crate::game::{TnE, GameState, Player, PlayerAction, Action, Leader, Movement};

pub struct TigrisAndEuphrates;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TnEMove {
    pub old_state: TnE,
    pub move_: Action,
}

unsafe impl Sync for TnEMove {}

impl Zobrist for TnE {
    fn zobrist_hash(&self) -> u64 {
        0
    }
}

impl TnEMove {
    pub fn new(old_state: TnE, action: Action) -> Self {
        Self {
            old_state,
            move_: action,
        }
    }
}

impl minimax::Move for TnEMove {
    type G = TigrisAndEuphrates;

    fn apply(&self, state: &mut <Self::G as minimax::Game>::S) {
        // dbg!(self.move_);
        state.process(self.move_).unwrap();
    }

    fn undo(&self, state: &mut <Self::G as minimax::Game>::S) {
        *state = self.old_state.clone();
    }
}

impl minimax::Game for TigrisAndEuphrates {
    type S = TnE;
    type M = TnEMove;

    fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
        let (_, action) = state.play_action_stack.last().unwrap();
        action.generate_moves(state, moves);
    }

    fn get_winner(state: &Self::S) -> Option<minimax::Winner> {
        if state.state == GameState::GameOver {
            match state.winner() {
                Player::None => Some(minimax::Winner::Draw),
                t if t == state.last_player => Some(minimax::Winner::PlayerJustMoved),
                _ => Some(minimax::Winner::PlayerToMove),
            }
        } else {
            None
        }
    }

    fn is_my_turn(state: &Self::S) -> bool {
        let (player, _) = state.play_action_stack.last().unwrap();
        *player == Player::Player1
    }
}

impl PlayerAction {
    pub(crate) fn generate_moves(&self, state: &TnE, moves: &mut Vec<TnEMove>) {
        let current_player = state.play_action_stack.last().map(|i|i.0).unwrap();

        match self {
            PlayerAction::AddSupport(tile_type) => {
                let max = state.players.get_mut(current_player).get_hand(*tile_type);
                for n in 0..=max {
                    moves.push(TnEMove::new(state.clone(), Action::AddSupport {tile_type: *tile_type, n}));
                }
            }
            PlayerAction::Normal => {
                // place tile
                let player = state.players.get_mut(current_player);
                for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].into_iter() {
                    if let Some(leader_pos) = player.get_leader(leader) {
                        if player.get_hand(leader.as_tile_type()) == 0 {
                            continue;
                        }

                        let spaces = state.board.find_empty_spaces_adj_kingdom(leader_pos, leader == Leader::Blue);
                        for to in spaces {
                            moves.push(TnEMove::new(state.clone(), Action::PlaceTile { to, tile_type: leader.as_tile_type() }));
                        }
                    }
                }

                // TODO: place tiles for places without leaders

                // TODO: Action::ReplaceTile(_))

                for pos in state.board.find_empty_leader_space_next_to_red() {
                    for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].into_iter() {
                        // if let Some(from) = state.players.get_mut(current_player).get_leader(leader) {
                        //     moves.push(TnEMove::new(state.clone(),
                        //         Action::MoveLeader { movement: Movement::Move{from, to: pos}, leader},
                        //     ));
                        // } else {
                            moves.push(TnEMove::new(state.clone(),
                                Action::MoveLeader { movement: Movement::Place(pos), leader},
                            ));
                        // }
                        // withdraw
                    }
                }

                // if state.players.get_mut(current_player).num_catastrophes > 0 {
                //     for pos in state.board.find_catastrophe_positions() {
                //         moves.push(TnEMove::new(state.clone(),
                //             Action::PlaceCatastrophe { to: pos },
                //         ));
                //     }
                // }

                // moves.push(TnEMove {
                //     old_state: state.clone(),
                //     move_: Action::Pass,
                // });
            }
            PlayerAction::SelectLeader { red, blue, green, black } => {
                if *red {
                    moves.push(TnEMove::new(state.clone(),
                        Action::WarSelectLeader { leader: Leader::Red },
                    ));
                }
                if *blue {
                    moves.push(TnEMove::new(
                        state.clone(),
                        Action::WarSelectLeader { leader: Leader::Blue },
                    ));
                }
                if *green {
                    moves.push(TnEMove::new(state.clone(),
                        Action::WarSelectLeader { leader: Leader::Green },
                    ));
                }
                if *black {
                    moves.push(TnEMove::new(state.clone(),
                        Action::WarSelectLeader { leader: Leader::Black },
                    ));
                }
            }
            PlayerAction::TakeTreasure(ts) => {
                moves.push(TnEMove::new(state.clone(),
                    Action::TakeTreasure{pos: ts[0]},
                ));
                moves.push(TnEMove::new(state.clone(),
                    Action::TakeTreasure{pos: ts[1]},
                ));
            }
        }
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
        let player = state.players.get_mut(Player::Player1);
        let s1 = player.calculate_score() as i16 + player.score_sum() as i16;
        let player = state.players.get_mut(Player::Player2);
        let s2 = player.calculate_score() as i16 + player.score_sum() as i16;

        if state.last_player == Player::Player1 {
            s2 - s1
        } else {
            s1 - s2
        }
    }
}