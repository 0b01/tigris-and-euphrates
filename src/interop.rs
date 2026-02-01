use numpy::ndarray::{Array3, Array1};
use numpy::{IntoPyArray, PyArray3, PyArray1};
use pyo3::types::PyTuple;
use pyo3::{pymodule, types::PyModule, PyResult, Python};
use crate::game::{TnEGame, Action, Pos, W, H, Movement, Leader, TileType, Player, Monument, RIVER, Bitboard, MonumentType, GameState};
use crate::solver::Evaluator;
use crate::pos;
use minimax::{Negamax, Strategy};

use pyo3::prelude::*;

#[warn(unused)]
#[repr(usize)]
enum Plane {
    TileRed,
    TileGreen,
    TileBlue,
    TileBlack,

    P1RedLeader,
    P1GreenLeader,
    P1BlueLeader,
    P1BlackLeader,

    P2RedLeader,
    P2GreenLeader,
    P2BlueLeader,
    P2BlackLeader,

    River,
    Treasure,
    Catastrophe,
    AnyMonument,
    UnificationTile,

    MonumentRed,
    MonumentGreen,
    MonumentBlue,
    MonumentBlack,

    CanPlaceTile,
    CanPlaceCatastrophe,
    CanPlaceLeader,
    IsConnectable,

    AdjKingdomCount0,
    AdjKingdomCount1,
    AdjKingdomCount2,
    AdjKingdomCount3,

    P1RedKingdom,
    P1GreenKingdom,
    P1BlueKingdom,
    P1BlackKingdom,

    P2RedKingdom,
    P2GreenKingdom,
    P2BlueKingdom,
    P2BlackKingdom,

    AllZeroes,
    AllOnes,

    LastLeaderMovement0,
    #[allow(unused)]
    LastLeaderMovement1,
    #[allow(unused)]
    LastLeaderMovement2,
    #[allow(unused)]
    LastLeaderMovement3,
    #[allow(unused)]
    LastLeaderMovement4,
    #[allow(unused)]
    LastLeaderMovement5,

    LastTilePlacement0,
    #[allow(unused)]
    LastTilePlacement1,
    #[allow(unused)]
    LastTilePlacement2,
    #[allow(unused)]
    LastTilePlacement3,
    #[allow(unused)]
    LastTilePlacement4,
    #[allow(unused)]
    LastTilePlacement5,

    Turn,
    Last,
}

#[pymethods]
impl TnEGame {
    #[new]
    fn new_game() -> Self {
        Self::new()
    }

    #[classattr]
    const W: usize = W;

    #[classattr]
    const H: usize = H;

    #[classattr]
    const N_CHANNELS: usize = Plane::Last as usize;

    #[classattr]
    const N_ACTIONS: usize = 11 * H * W + 6 + 1 + 7 + 4 + 1;

    #[pyo3(name="invalid_actions")]
    fn invalid_actions<'py>(&self, py: Python<'py>) -> &'py PyArray1<u8> {
        let mut z = Array1::ones(Self::N_ACTIONS);
        for m in self.next_action().generate_moves(self) {
            let i: usize = m.into();
            z[i] = 0;
        }
        z.into_pyarray(py)
    }

    #[pyo3(name="state")]
    fn state<'py>(&self, py: Python<'py>) -> &'py PyArray3<f64> {
        let mut z = Array3::zeros((Plane::Last as usize, H, W));
        for x in 0..H {
            for y in 0..W {
                let pos = pos!(x as u8, y as u8);
                z[[Plane::TileRed as usize, x, y]] = (self.board.get_tile_type(pos) == TileType::Red) as u8 as _;
                z[[Plane::TileGreen as usize, x, y]] = (self.board.get_tile_type(pos) == TileType::Green) as u8 as _;
                z[[Plane::TileBlue as usize, x, y]] = (self.board.get_tile_type(pos) == TileType::Blue) as u8 as _;
                z[[Plane::TileBlack as usize, x, y]] = (self.board.get_tile_type(pos) == TileType::Black) as u8 as _;

                z[[Plane::P1RedLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Red && self.board.get_player(pos) == Player::Player1) as u8 as _;
                z[[Plane::P1GreenLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Green && self.board.get_player(pos) == Player::Player1) as u8 as _;
                z[[Plane::P1BlueLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Blue && self.board.get_player(pos) == Player::Player1) as u8 as _;
                z[[Plane::P1BlackLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Black && self.board.get_player(pos) == Player::Player1) as u8 as _;

                z[[Plane::P2RedLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Red && self.board.get_player(pos) == Player::Player2) as u8 as _;
                z[[Plane::P2GreenLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Green && self.board.get_player(pos) == Player::Player2) as u8 as _;
                z[[Plane::P2BlueLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Blue && self.board.get_player(pos) == Player::Player2) as u8 as _;
                z[[Plane::P2BlackLeader as usize, x, y]] = (self.board.get_leader(pos) == Leader::Black && self.board.get_player(pos) == Player::Player2) as u8 as _;

                z[[Plane::River as usize, x, y]] = RIVER.get(pos) as u8 as _;
                z[[Plane::Treasure as usize, x, y]] = self.board.get_treasure(pos) as u8 as _;
                z[[Plane::Catastrophe as usize, x, y]] = (self.board.get_catastrophe(pos)) as u8 as _;
                z[[Plane::AnyMonument as usize, x, y]] = (self.board.get_monument(pos)) as u8 as _;

                z[[Plane::CanPlaceTile as usize, x, y]] = self.board.can_place_tile(pos) as u8 as _;
                z[[Plane::CanPlaceCatastrophe as usize, x, y]] = self.board.can_place_catastrophe(pos) as u8 as _;
                z[[Plane::IsConnectable as usize, x, y]] = self.board.is_connectable(pos) as u8 as _;

                z[[Plane::AllOnes as usize, x, y]] = 1 as _;
                z[[Plane::AllZeroes as usize, x, y]] = 0 as _;
                z[[Plane::Turn as usize, x, y]] = if self.next_player() == Player::Player1 {1} else {0} as _;
            }
        }

        for pos in self.board.find_empty_leader_space_next_to_red().iter() {
            z[[Plane::CanPlaceLeader as usize, pos.x as usize, pos.y as usize]] = 1 as _;
        }

        let count = self.board.nearby_kingdom_count();
        for x in 0..H {
            for y in 0..W {
                z[[Plane::AdjKingdomCount0 as usize, x, y]] = (count[x][y] == 0) as u8 as _;
                z[[Plane::AdjKingdomCount1 as usize, x, y]] = (count[x][y] == 1) as u8 as _;
                z[[Plane::AdjKingdomCount2 as usize, x, y]] = (count[x][y] == 2) as u8 as _;
                z[[Plane::AdjKingdomCount3 as usize, x, y]] = (count[x][y] >= 3) as u8 as _;
            }
        }

        macro_rules! kingdom {
            ($player:expr, $leader:expr, $var:ident) => {
                if let Some(pos) = self.players.get($player).get_leader($leader) {
                    let mut visited = Bitboard::new();
                    let kingdom = self.board.find_kingdom(pos, &mut visited);
                    for pos in kingdom.map.iter() {
                        z[[Plane::$var as usize, pos.x as usize, pos.y as usize]] = 1 as _;
                    }
                }
            };
        }
        kingdom!(Player::Player1, Leader::Red, P1RedKingdom);
        kingdom!(Player::Player1, Leader::Green, P1GreenKingdom);
        kingdom!(Player::Player1, Leader::Blue, P1BlueKingdom);
        kingdom!(Player::Player1, Leader::Black, P1BlackKingdom);
        kingdom!(Player::Player2, Leader::Red, P2RedKingdom);
        kingdom!(Player::Player2, Leader::Green, P2GreenKingdom);
        kingdom!(Player::Player2, Leader::Blue, P2BlueKingdom);
        kingdom!(Player::Player2, Leader::Black, P2BlackKingdom);

        if let Some(u) = self.board.unification_tile {
            z[[Plane::UnificationTile as usize, u.x as usize, u.y as usize]] = 1 as _;
        }

        for Monument {monument_type, pos_top_left} in self.monuments.iter() {
            macro_rules! monument {
                ($var:ident) => {
                    z[[Plane::$var as usize, pos_top_left.x as usize, pos_top_left.y as usize]] = 1.;
                    z[[Plane::$var as usize, pos_top_left.x as usize + 1, pos_top_left.y as usize]] = 1.;
                    z[[Plane::$var as usize, pos_top_left.x as usize, pos_top_left.y as usize + 1]] = 1.;
                    z[[Plane::$var as usize, pos_top_left.x as usize + 1, pos_top_left.y as usize + 1]] = 1.;
                };
            }
            if monument_type.matches(TileType::Red) { monument!(MonumentRed); }
            if monument_type.matches(TileType::Green) { monument!(MonumentGreen); }
            if monument_type.matches(TileType::Blue) { monument!(MonumentBlue); }
            if monument_type.matches(TileType::Black) { monument!(MonumentBlack); }
        }

        self.move_history.last_moved_leaders.iter().enumerate().for_each(|(i, pos)| {
            if let Some(pos) = pos {
                z[[Plane::LastLeaderMovement0 as usize + i, pos.x as usize, pos.y as usize]] = 1 as _;
            }
        });

        self.move_history.last_placed_tiles.iter().enumerate().for_each(|(i, pos)| {
            if let Some(pos) = pos {
                z[[Plane::LastTilePlacement0 as usize + i, pos.x as usize, pos.y as usize]] = 1 as _;
            }
        });

        z.into_pyarray(py)
    }

    /// Process an action
    #[pyo3(name="process")]
    fn process_action_vec<'py>(&mut self,
        py: Python<'py>,
        n: usize,
    ) -> &'py PyTuple {
        let action = n.into();
        // dbg!(action);
        let curr_player = self.next_player();
        let success = self.process(action).is_ok();
        let next_player = self.next_player();

        let flip = curr_player != next_player;

        let game_over = self.state.is_empty();
        let reward: f32 = if game_over {
            match self.winner() {
                Player::Player1 => 1.,
                Player::Player2 => -1.,
                Player::None => 0.,
            }
        } else {
            0.
        };

        return PyTuple::new(py, vec![
            (success as u8 as f32),
            reward,
            (flip as u8 as f32),
        ]);
    }

    /// Get the AI's recommended move as an action index
    #[pyo3(name="get_ai_move")]
    fn get_ai_move(&self, depth: usize) -> Option<usize> {
        let mut strategy = Negamax::new(Evaluator::default(), depth as u8);
        strategy.choose_move(&self).map(|m: Action| m.into())
    }

    /// Get human-readable game state for BGA parsing
    #[pyo3(name="get_board_state")]
    fn get_board_state(&self) -> String {
        let mut result = String::new();
        
        // Board tiles and leaders
        result.push_str("BOARD:\n");
        for y in 0..H {
            for x in 0..W {
                let pos = pos!(x as u8, y as u8);
                let c = if self.board.get_catastrophe(pos) {
                    'X'
                } else if self.board.get_leader(pos) != Leader::None {
                    let p = self.board.get_player(pos);
                    let l = self.board.get_leader(pos);
                    match (p, l) {
                        (Player::Player1, Leader::Red) => '1',
                        (Player::Player1, Leader::Green) => '2',
                        (Player::Player1, Leader::Blue) => '3',
                        (Player::Player1, Leader::Black) => '4',
                        (Player::Player2, Leader::Red) => '5',
                        (Player::Player2, Leader::Green) => '6',
                        (Player::Player2, Leader::Blue) => '7',
                        (Player::Player2, Leader::Black) => '8',
                        _ => '?',
                    }
                } else {
                    match self.board.get_tile_type(pos) {
                        TileType::Red => 'r',
                        TileType::Green => 'g',
                        TileType::Blue => 'b',
                        TileType::Black => 'k',
                        TileType::Empty => if RIVER.get(pos) { '~' } else { '.' },
                    }
                };
                result.push(c);
            }
            result.push('\n');
        }
        
        // Player scores
        let p1 = self.players.get(Player::Player1);
        let p2 = self.players.get(Player::Player2);
        result.push_str(&format!("P1: r={} g={} b={} k={} t={}\n", 
            p1.score_red, p1.score_green, p1.score_blue, p1.score_black, p1.score_treasure));
        result.push_str(&format!("P2: r={} g={} b={} k={} t={}\n",
            p2.score_red, p2.score_green, p2.score_blue, p2.score_black, p2.score_treasure));
        
        // Current player and state
        result.push_str(&format!("TURN: {:?}\n", self.next_player()));
        if let Some(state) = self.state.last() {
            result.push_str(&format!("STATE: {:?}\n", state));
        }
        
        result
    }

    /// Get all legal moves as (action_index, description) pairs
    #[pyo3(name="get_legal_moves")]
    fn get_legal_moves(&self) -> Vec<(usize, String)> {
        self.next_action().generate_moves(self)
            .into_iter()
            .map(|m| {
                let idx: usize = m.into();
                let desc = format!("{:?}", m);
                (idx, desc)
            })
            .collect()
    }

    /// Check if game is over
    #[pyo3(name="is_game_over")]
    fn is_game_over(&self) -> bool {
        self.state.is_empty()
    }

    /// Get current player (1 or 2)
    #[pyo3(name="current_player")]
    fn current_player(&self) -> u8 {
        match self.next_player() {
            Player::Player1 => 1,
            Player::Player2 => 2,
            _ => 0,
        }
    }
}

#[pymodule]
fn tne(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<TnEGame>()?;

    Ok(())
}