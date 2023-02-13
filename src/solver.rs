use minimax::Zobrist;

use crate::game::{TnEGame, GameState, Player, PlayerAction, Action, Leader, Movement, H, W};

pub struct TigrisAndEuphrates;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TnEMove {
    pub old_state: TnEGame,
    pub move_: Action,
}

unsafe impl Sync for TnEMove {}

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
        // dbg!(self.move_);
        state.process(self.move_).unwrap();
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
        action.generate_moves(state, moves);
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
    pub(crate) fn generate_moves(&self, state: &TnEGame, moves: &mut Vec<TnEMove>) {
        let current_player = state.next_player();

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
                            if state.board.neighboring_kingdoms(to).len() as u8 > 2 {
                                continue;
                            }

                            moves.push(TnEMove::new(state.clone(), Action::PlaceTile { to, tile_type: leader.as_tile_type() }));
                        }
                    }
                }

                // TODO: place tiles for places without leaders

                // TODO: Action::ReplaceTile(_))

                for pos in state.board.find_empty_leader_space_next_to_red() {
                    let mut visited = [[false; W]; H];
                    let kingdom = state.board.find_kingdom(pos, &mut visited);
                    for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].into_iter() {
                        if let Some(from) = state.players.get_mut(current_player).get_leader(leader) {
                            // only move when the position is in enemy's kingdom 
                            if let Some(possible_enemy) = kingdom.get_leader_info(leader).map(|l| l.0) {
                                if possible_enemy != current_player {
                                    moves.push(TnEMove::new(state.clone(),
                                        Action::MoveLeader { movement: Movement::Move{from, to: pos}, leader},
                                    ));
                                }
                            }
                        } else {
                            moves.push(TnEMove::new(state.clone(),
                                Action::MoveLeader { movement: Movement::Place(pos), leader},
                            ));
                        }

                        // TODO: withdraw
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
                    Action::TakeTreasure(ts[0]),
                ));
                moves.push(TnEMove::new(state.clone(),
                    Action::TakeTreasure(ts[1]),
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
        let calculate_score = |p: Player| -> i16 {
            let player_state = state.players.get_mut(p);
            player_state.get_eval(state)
        };

        let s1 = calculate_score(Player::Player1);
        let s2 = calculate_score(Player::Player2);

        if state.last_player == Player::Player1 {
            s2 - s1
        } else {
            s1 - s2
        }
    }
}