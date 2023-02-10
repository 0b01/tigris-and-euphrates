// game for tigris and euphrates
use packed_struct::prelude::*;

const W: usize = 16;
const H: usize = 11;

#[derive(Debug)]
pub struct Game {
    board: Board,
    player_1: PlayerState,
    player_2: PlayerState,
    current_player: Player,
    bag: Tiles,
    
    /// Revolt
    internal_conflict: Option<Conflict>,

    /// War
    external_conflict: Option<ExternalConflict>,
}

pub struct Kingdom {
    red_leader: Option<(Player, Pos)>,
    black_leader: Option<(Player, Pos)>,
    green_leader: Option<(Player, Pos)>,
    blue_leader: Option<(Player, Pos)>,

    red_tiles: u8,
    black_tiles: u8,
    green_tiles: u8,
    blue_tiles: u8,
}

impl Kingdom {
    fn has_leader(&self) -> bool {
        self.red_leader.is_some() || self.black_leader.is_some() || self.green_leader.is_some() || self.blue_leader.is_some()
    }

    fn get_player_of_leader(&self, leader: Leader) -> Option<Player> {
        match leader {
            Leader::Red => self.red_leader.map(|(p, _)| p),
            Leader::Black => self.black_leader.map(|(p, _)| p),
            Leader::Green => self.green_leader.map(|(p, _)| p),
            Leader::Blue => self.blue_leader.map(|(p, _)| p),
            Leader::None => None,
        }
    }
}


impl Game {
    pub fn new() -> Self {
        let mut game = Game {
            board: Board::new(),
            player_1: PlayerState::new(),
            player_2: PlayerState::new(),
            current_player: Player::Player1,
            bag: Tiles {
                red: 57,
                black: 30,
                green: 30,
                blue: 36,
            },
            internal_conflict: None,
            external_conflict: None,
        };

        // initial draw for players, each player draws 6 tiles
        game.bag.player_draw(&mut game.player_1, 6);
        game.bag.player_draw(&mut game.player_2, 6);

        game
    }

    #[must_use]
    pub fn validate_action(&self, action: Action) -> Result<()> {
        let curr_player = match self.current_player {
            Player::None => panic!("Invalid current player"),
            Player::Player1 => self.player_1,
            Player::Player2 => self.player_2,
        };

        if self.internal_conflict.is_some() || self.external_conflict.is_some() {
            match action {
                Action::AddSupport { .. } => {},
                _ => return Err(Error::InConflict),
            }
        }

        match action {
            Action::AddSupport { conflict, n } => {
                // we must be in a conflict
                let conflict = match (self.internal_conflict.as_ref(), self.external_conflict.as_ref()) {
                    (Some(c), _) => c,
                    (_, Some(conflicts)) => {
                        let c = conflicts.conflicts.iter().find(|c| **c == conflict);
                        if c.is_none() {
                            return Err(Error::NoExternalConflict);
                        }
                        c.unwrap()
                    },
                    _ => return Err(Error::NotInConflict),
                };

                // the conflict must still be in progress
                // leaders must still be connected
                if self.path_find(conflict.attacker_pos, conflict.defender_pos) == false {
                    return Err(Error::LeadersDisconnected);
                }

                // the player must be the right one to send support
                if conflict.p1_sent_support && conflict.player_1 == self.current_player {
                    return Err(Error::AlreadySentSupport);
                }

                // the player must have the tile they are trying to add support with
                let tile_type = conflict.leader.to_tile_type();
                match tile_type {
                    TileType::Red => {
                        if curr_player.hand_red < n {
                            return Err(Error::NotEnoughTiles);
                        }
                    }
                    TileType::Black => {
                        if curr_player.hand_black < n {
                            return Err(Error::NotEnoughTiles);
                        }
                    }
                    TileType::Green => {
                        if curr_player.hand_green < n {
                            return Err(Error::NotEnoughTiles);
                        }
                    }
                    TileType::Blue => {
                        if curr_player.hand_blue < n {
                            return Err(Error::NotEnoughTiles);
                        }
                    }
                    TileType::Empty => panic!("Invalid tile type"),
                }
            }
            Action::PickLeader { leader } => {
                // we must be in an external conflict
                let Some(conflicts) = &self.external_conflict else {
                    return Err(Error::NoExternalConflict);
                };

                // the leader must still be in conflict
                let Some(conflict) = conflicts.conflicts.iter().find(|c| c.leader == leader) else {
                    return Err(Error::LeaderNotInConflict);
                };

                // leaders must still be connected
                if self.path_find(conflict.attacker_pos, conflict.defender_pos) == false {
                    return Err(Error::LeadersDisconnected);
                }
            }
            Action::PlaceCatastrophe { to } => {
                if curr_player.num_catastrophes == 0 {
                    return Err(Error::NoCatastrophes);
                }
                // cannot be placed if the destination contains a leader
                let cell = self.board.get(to);
                if cell.leader != Leader::None {
                    return Err(Error::CannotPlaceOverLeader);
                }
            }
            Action::PlaceTile { to, tile_type } => {
                let cell = self.board.get(to);
                if cell.tile_type != TileType::Empty {
                    return Err(Error::CannotPlaceTileOverTile);
                }
                match tile_type {
                    TileType::Blue => {
                        if curr_player.hand_blue == 0 {
                            return Err(Error::NotEnoughTiles);
                        }
                    },
                    TileType::Green => {
                        if curr_player.hand_green == 0 {
                            return Err(Error::NotEnoughTiles);
                        }
                    },
                    TileType::Red => {
                        if curr_player.hand_red == 0 {
                            return Err(Error::NotEnoughTiles);
                        }
                    },
                    TileType::Black => {
                        if curr_player.hand_black == 0 {
                            return Err(Error::NotEnoughTiles);
                        }
                    },
                    TileType::Empty => return Err(Error::InvalidTileType),
                }

                // only blue can be and must be placed on river
                let cell = self.board.get(to);
                let is_river = cell.terrain == Terrain::River;
                if let TileType::Blue = tile_type {
                    if is_river == false {
                        return Err(Error::MustPlaceBlueOnRiver);
                    }
                } else {
                    if is_river {
                        return Err(Error::CannotPlaceNonBlueOnRiver);
                    }
                } 

                // cannot join three kingdoms
                if self.neighboring_kingdoms(to).len() as u8 > 2 {
                    return Err(Error::CannotJoinThreeKingdoms);
                }
            },
            Action::Leader { movement: ty, leader } => {
                match ty {
                    Movement::Place(to) => {
                        self.check_leader_placement(to)?;
                    },
                    Movement::Move{from, to} => {
                        let from_cell = self.board.get(from);
                        let to_cell = self.board.get(to);
                        if from_cell.leader != leader {
                            return Err(Error::CannotMoveOtherLeader);
                        }
                        if from_cell.player != self.current_player {
                            return Err(Error::CannotMoveOtherLeader);
                        }
                        if to_cell.leader != Leader::None {
                            return Err(Error::CannotPlaceOverLeader);
                        }

                        self.check_leader_placement(to)?;
                    },
                    Movement::Withdraw(pos) => {
                        let cell = self.board.get(pos);
                        if cell.leader != leader {
                            return Err(Error::CannotMoveOtherLeader);
                        }
                    },
                }
            },
            Action::ReplaceTile(Tiles{red, black, green, blue}) => {
                // confirm that the player has enough tiles
                if curr_player.hand_red < red || curr_player.hand_black < black || curr_player.hand_green < green || curr_player.hand_blue < blue {
                    return Err(Error::NotEnoughTiles);
                }
            },
            Action::Pass => {},
        }

        Ok(())
    }

    pub fn process_action(&mut self, action: Action) -> PlayerActionSelection {
        let curr_player = self.current_player();

        let mut next_action = PlayerAction::Any;
        let mut next_player = self.current_player;
        let mut redraw = false;
        if let Action::AddSupport { .. } = action {
            //
        } else {
            let end_turn = curr_player.num_actions;
            if end_turn == 1 {
                next_player = match self.current_player {
                    Player::None => panic!("Invalid current player"),
                    Player::Player1 => Player::Player2,
                    Player::Player2 => Player::Player1,
                };
                curr_player.num_actions = 0;
                redraw = true;
            } else {
                curr_player.num_actions = 1;
            }
        }

        match action {
            Action::PickLeader { leader } => {
                let Some(conflicts) = &mut self.external_conflict else {
                    panic!("Invalid external conflict");
                };
                let Some(conflict) = conflicts.conflicts.iter_mut().find(|c| c.leader == leader) else {
                    panic!("Invalid external conflict");
                };

                conflict.leader = Leader::None;
                conflict.attacker_pos = conflict.defender_pos;
                conflict.defender_pos = conflict.attacker_pos;
                conflict.p1_support = 0;
                conflict.p2_support = 0;
            }
            Action::AddSupport { conflict, n } => {
                let c = match (self.internal_conflict.as_mut(), self.external_conflict.as_mut()) {
                    (Some(c), _) if c == &conflict => c,
                    (_, Some(conflicts)) => {
                        let Some(c) = conflicts.conflicts.iter_mut().find(|c| **c == conflict) else {
                            panic!("Invalid external conflict");
                        };
                        c
                    },
                    _ => panic!("Invalid conflict"),
                };
                

                if c.player_1 == self.current_player {
                    c.p1_support += n;
                    c.p1_sent_support = true;
                } else {
                    c.p2_support += n;
                    c.p2_sent_support = true;
                }

                match c.leader.to_tile_type() {
                    TileType::Red => curr_player.hand_red -= n,
                    TileType::Black => curr_player.hand_black -= n,
                    TileType::Green => curr_player.hand_green -= n,
                    TileType::Blue => curr_player.hand_blue -= n,
                    TileType::Empty => panic!("Invalid tile type"),
                }

                if c.all_sent() {
                    // TODO: resolve conflict
                    next_action = PlayerAction::Any;
                    next_player = match c.player_1 {
                        Player::Player1 if self.player_1.num_actions == 2 => Player::Player2,
                        Player::Player2 if self.player_2.num_actions == 2 => Player::Player1,
                        _ => c.player_1,
                    };

                    // defender draws back to 6
                    match c.player_2 {
                        Player::Player1 => self.bag.player_draw_to_6(&mut self.player_1),
                        Player::Player2 => self.bag.player_draw_to_6(&mut self.player_2),
                        _ => panic!("Invalid player"),
                    }

                    self.internal_conflict = None;
                    self.external_conflict = None;
                } else {
                    next_action = PlayerAction::AddTile(c.leader.to_tile_type());
                    next_player = c.player_2;
                }
            }
            Action::PlaceCatastrophe { to } => {
                let cell = self.board.get(to);
                cell.terrain = Terrain::Catastrophe;
                curr_player.num_catastrophes -= 1;
            }
            Action::PlaceTile { to, tile_type } => {
                let cell = self.board.get(to);
                // get kingdoms around the to-be-placed tile
                let kingdoms = self.neighboring_kingdoms(to);
                // 1. find if there's an external conflict, if it would unite kingdoms with same color leaders
                let mut blue_exists = false;
                let mut red_exists = false;
                let mut green_exists = false;
                let mut black_exists = false;
                let mut external_conflicts = vec![];
                for k in &kingdoms {
                    match (k.blue_leader, blue_exists) {
                        (Some(_), false) => blue_exists = true,
                        (Some((player, leader_pos)), true) => external_conflicts.push(Conflict {
                            player_1: self.current_player,
                            player_2: player,
                            leader: Leader::Blue,
                            attacker_pos: leader_pos,
                            defender_pos: to,
                            p1_support: 0,
                            p2_support: 0,
                            p1_sent_support: false,
                            p2_sent_support: false,
                        }),
                        (None, _) => (),
                    }
                    match (k.red_leader, red_exists) {
                        (Some(_), false) => red_exists = true,
                        (Some((player, leader_pos)), true) => external_conflicts.push(Conflict {
                            player_1: self.current_player,
                            player_2: player,
                            leader: Leader::Red,
                            attacker_pos: leader_pos,
                            defender_pos: to,
                            p1_support: 0,
                            p2_support: 0,
                            p1_sent_support: false,
                            p2_sent_support: false,
                        }),
                        (None, _) => (),
                    }
                    match (k.green_leader, green_exists) {
                        (Some(_), false) => green_exists = true,
                        (Some((player, leader_pos)), true) => external_conflicts.push(Conflict {
                            player_1: self.current_player,
                            player_2: player,
                            leader: Leader::Green,
                            attacker_pos: leader_pos,
                            defender_pos: to,
                            p1_support: 0,
                            p2_support: 0,
                            p1_sent_support: false,
                            p2_sent_support: false,
                        }),
                        (None, _) => (),
                    }
                    match (k.black_leader, black_exists) {
                        (Some(_), false) => black_exists = true,
                        (Some((player, leader_pos)), true) => external_conflicts.push(Conflict {
                            player_1: self.current_player,
                            player_2: player,
                            leader: Leader::Black,
                            attacker_pos: leader_pos,
                            defender_pos: to,
                            p1_support: 0,
                            p2_support: 0,
                            p1_sent_support: false,
                            p2_sent_support: false,
                        }),
                        (None, _) => (),
                    }
                }

                if !external_conflicts.is_empty() {
                    // start a conflict
                    let leaders = external_conflicts.iter().map(|c| c.leader).collect();
                    self.external_conflict = Some(ExternalConflict {
                        conflicts: external_conflicts,
                        unification_tile_pos: to,
                    });

                    // attacker can select which leader to resolve first
                    next_action = PlayerAction::SelectLeader(leaders);
                    next_player = self.current_player;
                } else {
                    // find the leader in the kingdom that would score this tile
                    cell.tile_type = tile_type;
                    match tile_type {
                        TileType::Blue => {
                            curr_player.hand_blue -= 1;
                        }
                        TileType::Green => {
                            curr_player.hand_green -= 1;
                        }
                        TileType::Red => {
                            curr_player.hand_red -= 1;
                        }
                        TileType::Black => {
                            curr_player.hand_black -= 1;
                        }
                        TileType::Empty => panic!("Invalid tile type"),
                    }
                    let scoring_player = kingdoms.iter().filter_map(|k| k.get_player_of_leader(tile_type.to_leader())).next();
                    match scoring_player {
                        Some(Player::Player1) => self.player_1.add_score(tile_type),
                        Some(Player::Player2) => self.player_2.add_score(tile_type),
                        Some(Player::None) => panic!("Invalid player"),
                        None => (),
                    }
                }
            },
            Action::Leader { movement, leader } => {
                match movement {
                    Movement::Place(pos) => {
                        let cell = self.board.get(pos);

                        // check internal conflict
                        // if the kingdom already contains a leader, then an internal conflict is triggered
                        let kingdoms = self.neighboring_kingdoms(pos);

                        let mut has_conflict = None;
                        for kingdom in kingdoms {
                            has_conflict = match leader {
                                Leader::Red if kingdom.red_leader.is_some() => kingdom.red_leader,
                                Leader::Black if kingdom.black_leader.is_some() => kingdom.black_leader,
                                Leader::Green if kingdom.green_leader.is_some() => kingdom.green_leader,
                                Leader::Blue if kingdom.blue_leader.is_some() => kingdom.blue_leader,
                                _ => None,
                            };
                        }

                        if let Some((player_2, p2_pos)) = has_conflict {
                            self.internal_conflict = Some(Conflict {
                                player_1: self.current_player,
                                player_2,
                                leader: leader,
                                attacker_pos: pos,
                                defender_pos: p2_pos,
                                p1_support: 0,
                                p2_support: 0,
                                p1_sent_support: false,
                                p2_sent_support: false,
                            });

                            next_action = PlayerAction::AddTile(leader.to_tile_type());
                        }

                        cell.leader = leader;
                        cell.player = self.current_player;
                        match leader {
                            Leader::Red => curr_player.has_red_leader = false,
                            Leader::Black => curr_player.has_black_leader = false,
                            Leader::Green => curr_player.has_green_leader = false,
                            Leader::Blue => curr_player.has_blue_leader = false,
                            _ => panic!("Invalid leader"),
                        }
                    },
                    Movement::Move{from, to} => {
                        let from_cell = self.board.get(from);
                        let to_cell = self.board.get(to);
                        from_cell.leader = Leader::None;
                        to_cell.player = from_cell.player;
                        from_cell.player = Player::None;
                        to_cell.leader = leader;
                    },
                    Movement::Withdraw(pos) => {
                        let cell = &mut self.board.0[pos.y as usize][pos.x as usize];
                        match cell.leader {
                            Leader::Red => curr_player.has_red_leader = true,
                            Leader::Black => curr_player.has_black_leader = true,
                            Leader::Green => curr_player.has_green_leader = true,
                            Leader::Blue => curr_player.has_blue_leader = true,
                            _ => panic!("Invalid leader"),
                        }
                        cell.leader = Leader::None;
                        cell.player = Player::None;
                    },
                }
            },
            Action::ReplaceTile(t @ Tiles{red, black, green, blue}) => {
                curr_player.hand_red -= red;
                curr_player.hand_black -= black;
                curr_player.hand_green -= green;
                curr_player.hand_blue -= blue;

                self.bag.player_draw(curr_player, t.sum());
            },
            Action::Pass => {},
        }

        // TODO: check if the game is over

        // redraw for the player
        if redraw {
            let curr_player = match self.current_player {
                Player::None => panic!("Invalid current player"),
                Player::Player1 => &mut self.player_1,
                Player::Player2 => &mut self.player_2,
            };
            self.bag.player_draw_to_6(curr_player);
        }
        self.current_player = next_player;
        PlayerActionSelection { next_player, next_action }
    }

    fn current_player(&self) -> &'static mut PlayerState {
        let curr_player = match self.current_player {
            Player::None => panic!("Invalid current player"),
            Player::Player1 => unsafe { &mut *(&self.player_1 as *const PlayerState as *mut PlayerState)}
            Player::Player2 => unsafe { &mut *(&self.player_2 as *const PlayerState as *mut PlayerState)}
        };
        curr_player
    }

    fn neighboring_kingdoms(&self, to: Pos) -> Vec<Kingdom> {
        let mut kingdoms = vec![];
        let mut visited = [[false; W]; H];
        for neighbor in to.neighbors() {
            if visited[neighbor.y as usize][neighbor.x as usize] {
                continue;
            }
            let k = self.board.find_kingdom(neighbor, &mut visited);
            if k.has_leader() {
                kingdoms.push(k);
            }
        }
        kingdoms
    }

    pub fn process(&mut self, action: Action) -> Result<PlayerActionSelection> {
        self.validate_action(action)?;
        Ok(self.process_action(action))
    }
    
    fn nearby_kingdoms_count(&self, pos: Pos) -> u8 {
        self.neighboring_kingdoms(pos).len() as u8
    }

    #[must_use]
    fn adjacent_to_temple(&self, pos: Pos) -> bool {
        for neighbor in pos.neighbors() {
            let cell = self.board.get(neighbor);
            if cell.tile_type == TileType::Red {
                return true;
            }
        }
        false
    }

    #[must_use]
    fn check_leader_placement(&self, pos: Pos) -> Result<()> {
        let cell = self.board.get(pos);
        if cell.leader != Leader::None {
            return Err(Error::CannotPlaceOverLeader);
        }

        if cell.tile_type != TileType::Empty {
            return Err(Error::CannotPlaceTileOverTile);
        }

        // must be adjacent to a temple
        if !self.adjacent_to_temple(pos) {
            return Err(Error::CannotPlaceLeaderNotAdjacentToTemple);
        }

        // cannot be placed on river or catastrophe
        if self.board.get(pos).terrain == Terrain::River {
            return Err(Error::CannotPlaceLeaderOnRiver);
        }

        if self.board.get(pos).terrain == Terrain::Catastrophe {
            return Err(Error::CannotPlaceLeaderOnCatastrophe);
        }

        // a leader cannot join two kingdoms
        if self.nearby_kingdoms_count(pos) > 1 {
            return Err(Error::CannotPlaceLeaderJoiningTwoKingdoms);
        }

        Ok(())
    }

    #[must_use]
    fn path_find(&self, p1_pos: Pos, p2_pos: Pos) -> bool {
        // find a path between p1_pos and p2_pos, the cells must be is_connectable()
        let mut visited = [[false; W]; H];
        let mut queue = vec![p1_pos];
        while let Some(pos) = queue.pop() {
            if pos == p2_pos {
                return true;
            }
            visited[pos.y as usize][pos.x as usize] = true;
            for neighbor in pos.neighbors() {
                if !visited[neighbor.y as usize][neighbor.x as usize] && self.board.get(pos).is_connectable() {
                    queue.push(neighbor);
                }
            }
        }

        false
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PlayerActionSelection {
    next_player: Player,
    next_action: PlayerAction,
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum PlayerAction {
    AddTile(TileType),
    SelectLeader(Vec<Leader>),
    Any,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternalConflict {
    conflicts: Vec<Conflict>,
    unification_tile_pos: Pos,
}


/// occurs when placing same leaders in same kingdom
#[derive(Debug, Clone, Copy, Eq)]
pub struct Conflict {
    player_1: Player,
    player_2: Player,
    leader: Leader,
    attacker_pos: Pos,
    defender_pos: Pos,
    p1_support: u8,
    p2_support: u8,
    p1_sent_support: bool,
    p2_sent_support: bool,
}
impl Conflict {
    fn all_sent(&self) -> bool {
        self.p1_sent_support && self.p2_sent_support
    }
}

impl PartialEq for Conflict {
    fn eq(&self, other: &Self) -> bool {
        // ignore support
        self.player_1 == other.player_1
        && self.player_2 == other.player_2
        && self.leader == other.leader
        && self.attacker_pos == other.attacker_pos
        && self.defender_pos == other.defender_pos
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Action {
    PickLeader { leader: Leader },
    AddSupport { conflict: Conflict, n: u8 },
    PlaceTile { to: Pos, tile_type: TileType },
    Leader { movement: Movement, leader: Leader},
    ReplaceTile(Tiles),
    PlaceCatastrophe { to: Pos },
    Pass,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Tiles {
    red: u8,
    black: u8,
    green: u8,
    blue: u8,
}
impl Tiles {
    fn sum(&self) -> u8 {
        self.black + self.blue + self.green + self.red
    }

    fn player_draw_to_6(&mut self, player: &mut PlayerState) {
        let n = 6 - player.hand_sum();
        self.player_draw(player, n);
    }

    fn player_draw(&mut self, player: &mut PlayerState, n: u8) {
        let drawn = self.draw(n);
        player.hand_red += drawn.red;
        player.hand_black += drawn.black;
        player.hand_green += drawn.green;
        player.hand_blue += drawn.blue;
    }

    /// randomly draw tiles from the bag(counters for 4 colors) without replacement:
    fn draw(&mut self, n: u8) -> Tiles {
        // random number between 0 and the number of tiles left in the bag
        let mut tiles = Tiles {
            red: 0,
            black: 0,
            green: 0,
            blue: 0,
        };

        for _ in 0..n {
            let mut rand = rand::random::<u8>();
            rand %= self.sum();
            if rand < self.red {
                tiles.red += 1;
                self.red -= 1;
            } else if rand < self.red + self.black {
                tiles.black += 1;
                self.black -= 1;
            } else if rand < self.red + self.black + self.green {
                tiles.green += 1;
                self.green -= 1;
            } else {
                tiles.blue += 1;
                self.blue -= 1;
            }
        }

        tiles
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Movement {
    Place(Pos),
    Move{from: Pos, to: Pos},
    Withdraw(Pos),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pos {
    pub x: u8,
    pub y: u8,
}

macro_rules! pos {
    ($x:expr, $y:expr) => {
        Pos {
            x: $x,
            y: $y,
        }
    };
}

impl Pos {
    pub fn neighbors(&self) -> Vec<Pos> {
        let mut ret = Vec::new();
        if self.x > 0 {
            ret.push(pos!(self.x - 1, self.y));
        }
        if self.x < W as u8 - 1 {
            ret.push(pos!(self.x + 1, self.y));
        }
        if self.y > 0 {
            ret.push(pos!(self.x, self.y - 1));
        }
        if self.y < H as u8 - 1 {
            ret.push(pos!(self.x, self.y + 1));
        }
        ret
    }
}

#[repr(packed)]
#[derive(Copy, Clone, Debug, Default)]
pub struct PlayerState {
    pub has_red_leader: bool,
    pub has_black_leader: bool,
    pub has_green_leader: bool,
    pub has_blue_leader: bool,

    pub hand_green: u8,
    pub hand_red: u8,
    pub hand_black: u8,
    pub hand_blue: u8,

    pub score_green: u8,
    pub score_red: u8,
    pub score_black: u8,
    pub score_blue: u8,

    /// false for 2, true for 1
    pub num_actions: u8,

    pub num_catastrophes: u8,
    score_treasure: u8,
}

impl PlayerState {
    pub fn new() -> Self {
        PlayerState {
            has_black_leader: true,
            has_red_leader: true,
            has_green_leader: true,
            has_blue_leader: true,

            hand_green: 0,
            hand_red: 0,
            hand_black: 0,
            hand_blue: 0,

            score_green: 0,
            score_red: 0,
            score_black: 0,
            score_blue: 0,
            score_treasure: 0,

            num_actions: 0,
            num_catastrophes: 2,
        }
    }

    /// get the score of the player
    /// which is the lowest score of the 4 colors
    /// treasure is the wildcard, add 1 treasure to lowest color, until we run out of treasure
    pub fn calculate_score(&self) -> u8 {
        let mut treasures = self.score_treasure;
        let mut current_lowest_color = TileType::Empty;
        let mut s = self.clone();

        while treasures > 0 {
            if s.score_green <= s.score_red && s.score_green <= s.score_black && s.score_green <= s.score_blue {
                current_lowest_color = TileType::Green;
            } else if s.score_black <= s.score_red && s.score_black <= s.score_green && s.score_black <= s.score_blue {
                current_lowest_color = TileType::Black;
            } else if s.score_red <= s.score_black && s.score_red <= s.score_green && s.score_red <= s.score_blue {
                current_lowest_color = TileType::Red;
            } else if s.score_blue <= s.score_black && s.score_blue <= s.score_green && s.score_blue <= s.score_red {
                current_lowest_color = TileType::Blue;
            }

            //assign 1 treasure to the lowest
            match current_lowest_color {
                TileType::Green => s.score_green += 1,
                TileType::Black => s.score_black += 1,
                TileType::Red => s.score_red += 1,
                TileType::Blue => s.score_blue += 1,
                _ => (),
            }

            treasures -= 1;
        }

        s.score_black.min(s.score_red).min(s.score_green).min(s.score_blue)
    }

    pub fn hand_sum(&self) -> u8 {
        self.hand_black + self.hand_red + self.hand_green + self.hand_blue
    }

    fn add_score(&mut self, tile_type: TileType) {
        match tile_type {
            TileType::Green => self.score_green += 1,
            TileType::Black => self.score_black += 1,
            TileType::Red => self.score_red += 1,
            TileType::Blue => self.score_blue += 1,
            _ => (),
        }
    }
}

#[derive(Debug)]
pub struct Board([[Cell; W]; H]);

impl Board {
    /// Create a new board
    pub fn new() -> Self {
        let mut ret = Board([[Cell::new(); W]; H]);
        let board = [
            "....xxxxx.t.x...",
            ".t..x.......x..t",
            "...xxt......xx..",
            "xxxx.........xxx",
            ".............txx",
            "..............x.",
            "xxxx....t...xxx.",
            ".t.xxxx.....x...",
            "......xxxxxxx.t.",
            ".....t..........",
            "..........t....."];
        for (y, row) in board.iter().enumerate() {
            for (x, c) in row.chars().enumerate() {
                match c {
                    'x' => ret.0[y][x].terrain = Terrain::River,
                    't' => {
                        ret.0[y][x].terrain = Terrain::Treasure;
                        ret.0[y][x].tile_type = TileType::Red;
                    },
                    '.' => {},
                    _ => panic!("Invalid board character"),
                }
            }
        }

        ret
    }

    pub fn get(&self, pos: Pos) -> &mut Cell {
        unsafe {
            &mut *(&self.0[pos.x as usize][pos.y as usize] as *const Cell as *mut Cell)
        }
    }

    fn find_kingdom(&self, pos: Pos, visited: &mut [[bool; W]; H]) -> Kingdom {
        let mut kingdom = Kingdom {
            red_leader: None,
            black_leader: None,
            green_leader: None,
            blue_leader: None,
            red_tiles: 0,
            black_tiles: 0,
            green_tiles: 0,
            blue_tiles: 0,
        };

        let mut stack = vec![pos];

        while let Some(pos) = stack.pop() {
            if visited[pos.y as usize][pos.x as usize] {
                continue;
            }
            visited[pos.y as usize][pos.x as usize] = true;


            let cell = self.get(pos);
            if !cell.is_connectable() {
                continue;
            }

            if cell.tile_type != TileType::Empty {
                match cell.tile_type {
                    TileType::Red => kingdom.red_tiles += 1,
                    TileType::Black => kingdom.black_tiles += 1,
                    TileType::Green => kingdom.green_tiles += 1,
                    TileType::Blue => kingdom.blue_tiles += 1,
                    TileType::Empty => panic!("Invalid tile type"),
                }
            }

            if cell.leader != Leader::None {
                match cell.leader {
                    Leader::Red => kingdom.red_leader = Some((cell.player, pos)),
                    Leader::Black => kingdom.black_leader = Some((cell.player, pos)),
                    Leader::Green => kingdom.green_leader = Some((cell.player, pos)),
                    Leader::Blue => kingdom.blue_leader = Some((cell.player, pos)),
                    Leader::None => panic!("Invalid leader"),
                }
            }

            for neighbor_pos in pos.neighbors() {
                stack.push(neighbor_pos);
            }
        }

        kingdom
    }

}

#[derive(PackedStruct, Copy, Clone, Debug)]
#[packed_struct(bit_numbering="msb0")]
pub struct Cell {
    #[packed_field(bits="0..=2", ty="enum")]
    tile_type: TileType,
    #[packed_field(bits="3..=5", ty="enum")]
    player: Player,
    #[packed_field(bits="6..=8", ty="enum")]
    leader: Leader,
    #[packed_field(bits="9..=10", ty="enum")]
    terrain: Terrain,
}

impl Cell {
    pub fn new() -> Self {
        Cell {
            tile_type: TileType::Empty,
            player: Player::None,
            leader: Leader::None,
            terrain: Terrain::None,
        }
    }

    fn is_connectable(&self) -> bool {
        self.tile_type != TileType::Empty || self.leader != Leader::None
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq, Eq)]
pub enum TileType {
    Empty,
    Blue,
    Green,
    Red,
    Black,
}
impl TileType {
    fn to_leader(&self) -> Leader {
        match self {
            TileType::Blue => Leader::Blue,
            TileType::Green => Leader::Green,
            TileType::Red => Leader::Red,
            TileType::Black => Leader::Black,
            TileType::Empty => Leader::None,
        }
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Player {
    None,
    Player1,
    Player2,
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Leader {
    None,
    Blue,
    Green,
    Red,
    Black,
}

impl Leader {
    fn to_tile_type(&self) -> TileType {
        match self {
            Leader::Blue => TileType::Blue,
            Leader::Green => TileType::Green,
            Leader::Red => TileType::Red,
            Leader::Black => TileType::Black,
            Leader::None => TileType::Empty,
        }
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum Terrain {
    None,
    Treasure,
    River,
    Catastrophe,
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NoExternalConflict,
    NotInConflict,
    NotEnoughTiles,
    LeadersDisconnected,
    LeaderNotInConflict,
    NoCatastrophes,
    CannotPlaceOverLeader,
    CannotPlaceTileOverTile,
    InvalidTileType,
    MustPlaceBlueOnRiver,
    CannotPlaceNonBlueOnRiver,
    CannotJoinThreeKingdoms,
    CannotPlaceLeaderNotAdjacentToTemple,
    CannotPlaceLeaderOnRiver,
    CannotPlaceLeaderOnCatastrophe,
    CannotPlaceLeaderJoiningTwoKingdoms,
    CannotMoveOtherLeader,
    InConflict,
    AlreadySentSupport,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pos_neighbors() {
        let pos = pos!(0, 0);
        let neighbors = pos.neighbors();
        assert_eq!(neighbors.len(), 2);
        assert!(neighbors.contains(&pos!(1, 0)));
        assert!(neighbors.contains(&pos!(0, 1)));
    }

    #[test]
    fn leader_must_be_placed_next_to_temples() {
        let mut game = Game::new();
        assert_eq!(game.current_player, Player::Player1);
        assert_eq!(game.player_1.hand_sum(), 6);
        let ret = game.process(Action::Leader { movement: Movement::Place(pos!(0, 0)), leader: Leader::Black });
        assert_eq!(ret.unwrap_err(), Error::CannotPlaceLeaderNotAdjacentToTemple);
    }

    #[test]
    fn leader_must_be_placed_next_to_temples_ok() {
        let mut game = Game::new();
        assert_eq!(game.current_player, Player::Player1);
        let ret = game.process(Action::Leader { movement: Movement::Place(pos!(0, 1)), leader: Leader::Black });
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player1, next_action: PlayerAction::Any });
        let cell = game.board.get(pos!(0, 1));
        assert_eq!(cell.leader, Leader::Black);
        assert_eq!(cell.player, Player::Player1);
        dbg!(cell);

        let ret = game.process(Action::Leader { movement: Movement::Place(pos!(1, 0)), leader: Leader::Red });
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player2, next_action: PlayerAction::Any });
        let cell = game.board.get(pos!(1, 0));
        assert_eq!(cell.leader, Leader::Red);
        assert_eq!(cell.player, Player::Player1);
    }

    fn ensure_player_has_at_least_1_color(player: &mut PlayerState, color: TileType) {
        match color {
            TileType::Blue => ensure_player_has_at_least_1_blue(player),
            TileType::Green => ensure_player_has_at_least_1_green(player),
            TileType::Red => ensure_player_has_at_least_1_red(player),
            TileType::Black => ensure_player_has_at_least_1_black(player),
            TileType::Empty => (),
        }

        fn ensure_player_has_at_least_1_blue(player: &mut PlayerState) {
            if player.hand_blue == 0 {
                player.hand_blue = 1;
    
                if player.hand_black > 0 {
                    player.hand_black -= 1;
                } else if player.hand_green > 0 {
                    player.hand_green -= 1;
                } else if player.hand_red > 0 {
                    player.hand_red -= 1;
                } 
            }
        }
        fn ensure_player_has_at_least_1_green(player: &mut PlayerState) {
            if player.hand_green == 0 {
                player.hand_green = 1;
    
                if player.hand_black > 0 {
                    player.hand_black -= 1;
                } else if player.hand_blue > 0 {
                    player.hand_blue -= 1;
                } else if player.hand_red > 0 {
                    player.hand_red -= 1;
                } 
            }
        }
        fn ensure_player_has_at_least_1_red(player: &mut PlayerState) {
            if player.hand_red == 0 {
                player.hand_red = 1;
    
                if player.hand_black > 0 {
                    player.hand_black -= 1;
                } else if player.hand_blue > 0 {
                    player.hand_blue -= 1;
                } else if player.hand_green > 0 {
                    player.hand_green -= 1;
                } 
            }
        }
        fn ensure_player_has_at_least_1_black(player: &mut PlayerState) {
            if player.hand_black == 0 {
                player.hand_black = 1;
    
                if player.hand_red > 0 {
                    player.hand_red -= 1;
                } else if player.hand_blue > 0 {
                    player.hand_blue -= 1;
                } else if player.hand_green > 0 {
                    player.hand_green -= 1;
                } 
            }
        }
    }

    #[test]
    fn internal_conflict() {
        let mut game = Game::new();

        // make sure we have 1 red tile for this test to work reliably
        ensure_player_has_at_least_1_color(&mut game.player_1, TileType::Red);
        ensure_player_has_at_least_1_color(&mut game.player_2, TileType::Red);

        game.process(Action::Leader { movement: Movement::Place(pos!(0, 1)), leader: Leader::Black }).unwrap();
        game.process(Action::Leader { movement: Movement::Place(pos!(1, 0)), leader: Leader::Red }).unwrap();

        assert_eq!(game.player_1.hand_sum(), 6);
        assert_eq!(game.player_2.hand_sum(), 6);

        assert_eq!(game.current_player, Player::Player2);
        let ret = game.process(Action::Leader { movement: Movement::Place(pos!(2, 1)), leader: Leader::Red });
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player2, next_action: PlayerAction::AddTile(TileType::Red) });
        assert!(game.internal_conflict.is_some());
        assert_eq!(game.internal_conflict.unwrap(), Conflict {
            player_1: Player::Player2,
            player_2: Player::Player1,
            leader: Leader::Red,
            attacker_pos: pos!(2, 1),
            defender_pos: pos!(1, 0),
            p1_support: 0,
            p2_support: 0,
            p1_sent_support: false,
            p2_sent_support: false,
        });

        let ret = game.process(Action::Leader { movement: Movement::Place(pos!(2, 1)), leader: Leader::Red });
        assert_eq!(ret.unwrap_err(), Error::InConflict);

        ensure_player_has_at_least_1_color(&mut game.player_2, TileType::Black);
        let ret = game.process(Action::AddSupport { conflict: game.internal_conflict.unwrap(), n: 6 });
        assert_eq!(ret.unwrap_err(), Error::NotEnoughTiles);
        assert_eq!(game.player_2.hand_sum(), 6);

        let ret = game.process(Action::AddSupport { conflict: game.internal_conflict.unwrap(), n: 1 });
        assert_eq!(game.player_2.hand_sum(), 5);
        assert_eq!(game.internal_conflict.unwrap().p1_support, 1);
        assert_eq!(game.internal_conflict.unwrap().p2_support, 0);
        assert_eq!(game.internal_conflict.unwrap().p1_sent_support, true);
        assert_eq!(game.internal_conflict.unwrap().p2_sent_support, false);
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player1, next_action: PlayerAction::AddTile(TileType::Red) });

        let ret = game.process(Action::AddSupport { conflict: game.internal_conflict.unwrap(), n: 1 });
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player2, next_action: PlayerAction::Any });
        assert_eq!(game.player_2.num_actions, 1);
        assert_eq!(game.player_1.num_actions, 0);

        assert_eq!(game.player_2.hand_sum(), 5);
        assert_eq!(game.player_1.hand_sum(), 6); // defender draws

        let ret = game.process(Action::Pass);
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player1, next_action: PlayerAction::Any });

        assert_eq!(game.player_2.hand_sum(), 6);
        assert_eq!(game.player_1.hand_sum(), 6);
    }

    #[test]
    fn score_is_correct() {
        let mut player = PlayerState::default();
        player.score_black = 1;
        player.score_blue = 2;
        player.score_green = 3;
        player.score_red = 4;
        assert_eq!(player.calculate_score(), 1);
        player.score_treasure = 1;
        assert_eq!(player.calculate_score(), 2);
        player.score_treasure = 3;
        assert_eq!(player.calculate_score(), 3);
    }

    #[test]
    fn place_tile() {
        let mut game = Game::new();
        game.process(Action::Leader{movement: Movement::Place(pos!(0, 1)), leader: Leader::Red}).unwrap();
        ensure_player_has_at_least_1_color(&mut game.player_1, TileType::Red);
        let ret = game.process(Action::PlaceTile { to: pos!(0, 0), tile_type: TileType::Red });
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player2, next_action: PlayerAction::Any });
        assert_eq!(game.board.get(pos!(0, 0)).tile_type, TileType::Red);
        assert_eq!(game.player_2.hand_sum(), 6);
        assert_eq!(game.player_1.hand_sum(), 6);
        assert_eq!(game.player_1.score_red, 1);
    }

    #[test]
    fn place_tile_no_leader() {
        let mut game = Game::new();
        ensure_player_has_at_least_1_color(&mut game.player_1, TileType::Red);
        let ret = game.process(Action::PlaceTile { to: pos!(0, 0), tile_type: TileType::Red });
        assert_eq!(ret.unwrap(), PlayerActionSelection { next_player: Player::Player1, next_action: PlayerAction::Any });
        assert_eq!(game.board.get(pos!(0, 0)).tile_type, TileType::Red);
        assert_eq!(game.player_2.hand_sum(), 6);
        assert_eq!(game.player_1.hand_sum(), 5);
        assert_eq!(game.player_1.score_red, 0);
    }

    #[test]
    fn place_tile_score_enemy_leader() {
        let mut game = Game::new();
        game.process(Action::Leader{movement: Movement::Place(pos!(0, 1)), leader: Leader::Red}).unwrap();
        game.process(Action::Leader{movement: Movement::Place(pos!(1, 0)), leader: Leader::Black}).unwrap();
        assert_eq!(game.current_player, Player::Player2);
        ensure_player_has_at_least_1_color(&mut game.player_2, TileType::Red);
        game.process(Action::PlaceTile { to: pos!(0, 0), tile_type: TileType::Red }).unwrap();
        ensure_player_has_at_least_1_color(&mut game.player_2, TileType::Black);
        game.process(Action::PlaceTile { to: pos!(2, 0), tile_type: TileType::Black }).unwrap();
        assert_eq!(game.player_2.hand_sum(), 6);
        assert_eq!(game.player_1.hand_sum(), 6);

        assert_eq!(game.player_1.score_red, 1);
        assert_eq!(game.player_1.score_black, 1);
    }

    #[test]
    fn place_tile_trigger_external_conflict() {
        let mut game = Game::new();
        ensure_player_has_at_least_1_color(&mut game.player_1, TileType::Red);
        game.process(Action::Leader{movement: Movement::Place(pos!(1, 0)), leader: Leader::Red}).unwrap();
        game.process(Action::PlaceTile { to: pos!(0, 2), tile_type: TileType::Red }).unwrap();
        assert_eq!(game.board.get(pos!(0, 2)).tile_type, TileType::Red);
        assert_eq!(game.player_1.score_red, 0);
        game.process(Action::Leader { movement: Movement::Place(pos!(0, 3)), leader: Leader::Red }).unwrap();
        ensure_player_has_at_least_1_color(&mut game.player_2, TileType::Red);
        let ret = game.process(Action::PlaceTile { to: pos!(0, 1), tile_type: TileType::Red }).unwrap();
        assert_eq!(ret.next_player, Player::Player2);
        assert_eq!(ret.next_action, PlayerAction::SelectLeader(vec![Leader::Red]));
        assert_eq!(game.external_conflict, Some(ExternalConflict { conflicts: vec![
            Conflict {
                player_1: Player::Player2,
                player_2: Player::Player2,
                leader: Leader::Red,
                attacker_pos: pos!(0, 3),
                defender_pos: pos!(0, 1),
                p1_support: 0,
                p2_support: 0,
                p1_sent_support: false,
                p2_sent_support: false
            }],
            unification_tile_pos: pos!(0, 1)
        }));
    }
}