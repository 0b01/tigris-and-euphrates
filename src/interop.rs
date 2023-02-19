use numpy::ndarray::{Array3};
use numpy::{IntoPyArray, PyArray3};
use pyo3::{pymodule, types::PyModule, PyResult, Python};
use crate::game::{TnEGame, Action, Pos, W, H, Tiles, Movement, Leader, TileType, Player, Monument, RIVER, Bitboard};
use crate::pos;

use pyo3::prelude::*;

fn from_action_vector(action: u8, from: Pos, to: Pos, tiles: Tiles, ty: u8) -> Action {
    let action = match action {
        0 => Action::WarSelectLeader { leader: ty.into() },
        1 => Action::AddSupport { tile_type: ty.into(), n: tiles.count(ty.into()) },
        2 => Action::BuildMonument { monument_type: ty.into(), pos_top_left: to },
        3 => Action::PlaceTile { to, tile_type: ty.into() },
        4 => Action::TakeTreasure(to),
        5 => Action::MoveLeader { movement: Movement::Place(to), leader: ty.into()},
        6 => Action::MoveLeader { movement: Movement::Move { from, to }, leader: ty.into()},
        7 => Action::MoveLeader { movement: Movement::Withdraw(to), leader: ty.into()},
        8 => Action::ReplaceTile(tiles),
        9 => Action::PlaceCatastrophe { to },
        10 => Action::Pass,
        _ => panic!(),
    };

    dbg!(&action);

    action
}

#[deny(unused)]
#[repr(usize)]
pub enum Plane {
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
    LastLeaderMovement1,
    LastLeaderMovement2,
    LastLeaderMovement3,
    LastLeaderMovement4,
    LastLeaderMovement5,

    LastTilePlacement0,
    LastTilePlacement1,
    LastTilePlacement2,
    LastTilePlacement3,
    LastTilePlacement4,
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
    const NACTIONS: usize = std::mem::variant_count::<Action>();

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

    #[pyo3(name="process")]
    /// Process an action, the args are multiuse and depend on the action
    fn process_action_vec(&mut self,
        action: u8,

        from_x: u8,
        from_y: u8,
        to_x: u8,
        to_y: u8,

        red: u8,
        green: u8,
        blue: u8,
        black: u8,

        leader: u8,
    ) -> pyo3::PyResult<()> {
        let action = from_action_vector(
            action,
            pos!(from_x, from_y),
            pos!(to_x, to_y),
            Tiles{red, green, blue, black},
            leader,
        );
        self.process(action).unwrap();
        Ok(())
    }
}

#[pymodule]
fn tne(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<TnEGame>()?;

    Ok(())
}