// game for tigris and euphrates
use packed_struct::prelude::*;

const W: usize = 16;
const H: usize = 11;

#[derive(Debug)]
pub struct Players([PlayerState; 2]);
impl Players {
    pub fn get_mut(&self, player: Player) -> &mut PlayerState {
        match player {
            Player::Player1 => unsafe {
                &mut *(&self.0[0] as *const PlayerState as *mut PlayerState)
            },
            Player::Player2 => unsafe {
                &mut *(&self.0[1] as *const PlayerState as *mut PlayerState)
            },
            Player::None => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Game {
    board: Board,
    players: Players,
    bag: Tiles,

    state: GameState,

    play_action_stack: Vec<(Player, PlayerAction)>,
    player_turn: Player,

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
    treasures: [Option<Pos>; 2],
}

impl Kingdom {
    fn has_leader(&self) -> bool {
        self.red_leader.is_some()
            || self.black_leader.is_some()
            || self.green_leader.is_some()
            || self.blue_leader.is_some()
    }

    fn get_leader_stats(&self, leader: Leader) -> Option<(Player, Pos, u8)> {
        match leader {
            Leader::Red => self.red_leader.map(|(p, pos)| (p, pos, self.red_tiles)),
            Leader::Black => self.black_leader.map(|(p, pos)| (p, pos, self.black_tiles)),
            Leader::Green => self.green_leader.map(|(p, pos)| (p, pos, self.green_tiles)),
            Leader::Blue => self.blue_leader.map(|(p, pos)| (p, pos, self.blue_tiles)),
            Leader::None => None,
        }
    }
}

macro_rules! p1 {
    () => {
        (Player::Player1, PlayerAction::Any)
    };
    ($i:expr) => {
        (Player::Player1, $i)
    };
}

#[allow(unused)]
macro_rules! p2 {
    () => {
        (Player::Player2, PlayerAction::Any)
    };
    ($i:expr) => {
        (Player::Player2, $i)
    };
}

impl Default for Game {
    fn default() -> Self {
        Self::new()
    }
}

impl Game {
    pub fn new() -> Self {
        let mut game = Game {
            board: Board::new(),
            players: Players([PlayerState::new(); 2]),
            state: GameState::Normal,
            bag: Tiles {
                red: 57,
                black: 30,
                green: 30,
                blue: 36,
            },
            play_action_stack: vec![p1!(), p1!()],
            player_turn: Player::Player1,
            internal_conflict: None,
            external_conflict: None,
        };

        // initial draw for players, each player draws 6 tiles
        for player in &mut game.players.0 {
            game.bag.player_draw(player, 6);
        }

        game
    }

    pub fn validate_action(&self, action: Action, current_player: Player) -> Result<()> {
        let curr_player = self.players.get_mut(current_player);

        let is_valid = match (self.state, action) {
            (GameState::Normal, Action::PlaceTile { .. })
            | (GameState::Normal, Action::MoveLeader { .. })
            | (GameState::Normal, Action::ReplaceTile(_))
            | (GameState::Normal, Action::PlaceCatastrophe { .. })
            | (GameState::Normal, Action::Pass)
            | (GameState::TakeTreasure, Action::TakeTreasure { .. })
            | (GameState::WarSelectLeader, Action::WarSelectLeader { .. })
            | (GameState::AddSupport, Action::AddSupport { .. }) => true,
            _ => false,
        };

        if !is_valid {
            return Err(Error::InvalidAction(self.state));
        }

        match action {
            Action::TakeTreasure { pos } => {
                if self.board.get(pos).terrain != Terrain::Treasure {
                    return Err(Error::NoTreasure);
                }
            }
            Action::AddSupport { tile_type, n } => {
                // we must be in a conflict
                let conflict = match (
                    self.internal_conflict.as_ref(),
                    self.external_conflict.as_ref(),
                ) {
                    (Some(c), _) => c,
                    (_, Some(conflicts)) => {
                        let c = conflicts
                            .conflicts
                            .iter()
                            .find(|c| c.leader.as_tile_type() == tile_type);
                        if c.is_none() {
                            return Err(Error::NoExternalConflict);
                        }
                        c.unwrap()
                    }
                    _ => return Err(Error::NotInConflict),
                };

                // the conflict must still be in progress
                // leaders must still be connected
                if !self.path_find(conflict.attacker_pos, conflict.defender_pos) {
                    return Err(Error::LeadersDisconnected);
                }

                // the player must be the right one to send support
                if conflict.attacker_sent_support && conflict.attacker == current_player {
                    return Err(Error::AlreadySentSupport);
                }

                // the player must have the tile they are trying to add support with
                let tile_type = conflict.leader.as_tile_type();
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
                    TileType::Empty => unreachable!(),
                }
            }
            Action::WarSelectLeader { leader } => {
                // we must be in an external conflict
                let Some(conflicts) = &self.external_conflict else {
                    return Err(Error::NoExternalConflict);
                };

                // the leader must still be in conflict
                let Some(conflict) = conflicts.conflicts.iter().find(|c| c.leader == leader) else {
                    return Err(Error::LeaderNotInConflict);
                };

                // leaders must still be connected
                if !self.path_find(conflict.attacker_pos, conflict.defender_pos) {
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
                    }
                    TileType::Green => {
                        if curr_player.hand_green == 0 {
                            return Err(Error::NotEnoughTiles);
                        }
                    }
                    TileType::Red => {
                        if curr_player.hand_red == 0 {
                            return Err(Error::NotEnoughTiles);
                        }
                    }
                    TileType::Black => {
                        if curr_player.hand_black == 0 {
                            return Err(Error::NotEnoughTiles);
                        }
                    }
                    TileType::Empty => return Err(Error::InvalidTileType),
                }

                // only blue can be and must be placed on river
                let cell = self.board.get(to);
                let is_river = cell.terrain == Terrain::River;
                if let TileType::Blue = tile_type {
                    if !is_river {
                        return Err(Error::MustPlaceBlueOnRiver);
                    }
                } else if is_river {
                    return Err(Error::CannotPlaceNonBlueOnRiver);
                }

                // cannot join three kingdoms
                if self.neighboring_kingdoms(to).len() as u8 > 2 {
                    return Err(Error::CannotJoinThreeKingdoms);
                }
            }
            Action::MoveLeader {
                movement: ty,
                leader,
            } => match ty {
                Movement::Place(to) => {
                    self.check_leader_placement(to)?;
                }
                Movement::Move { from, to } => {
                    let from_cell = self.board.get(from);
                    let to_cell = self.board.get(to);
                    if from_cell.leader != leader {
                        return Err(Error::CannotMoveOtherLeader);
                    }
                    if from_cell.player != current_player {
                        return Err(Error::CannotMoveOtherLeader);
                    }
                    if to_cell.leader != Leader::None {
                        return Err(Error::CannotPlaceOverLeader);
                    }

                    self.check_leader_placement(to)?;
                }
                Movement::Withdraw(pos) => {
                    let cell = self.board.get(pos);
                    if cell.leader != leader {
                        return Err(Error::CannotMoveOtherLeader);
                    }
                }
            },
            Action::ReplaceTile(Tiles {
                red,
                black,
                green,
                blue,
            }) => {
                // confirm that the player has enough tiles
                if curr_player.hand_red < red
                    || curr_player.hand_black < black
                    || curr_player.hand_green < green
                    || curr_player.hand_blue < blue
                {
                    return Err(Error::NotEnoughTiles);
                }
            }
            Action::Pass => {}
        }

        Ok(())
    }

    pub fn process_action(&mut self, action: Action, current_player: Player) -> GameState {
        let curr_player = self.players.get_mut(current_player);

        let mut next_state = self.state;

        match action {
            Action::TakeTreasure { pos } => {
                let cell = self.board.get(pos);
                cell.terrain = Terrain::Empty;

                curr_player.score_treasure += 1;
            }
            Action::WarSelectLeader { leader } => {
                assert!(self.internal_conflict.is_none());
                let Some(conflicts) = &mut self.external_conflict else {
                    unreachable!();
                };
                let Some(conflict) = conflicts.conflicts.iter_mut().find(|c| c.leader == leader) else {
                    unreachable!();
                };

                next_state = GameState::AddSupport;
                let next_action = PlayerAction::AddSupport(conflict.leader.as_tile_type());
                self.play_action_stack
                    .push((conflict.attacker, next_action));
            }
            Action::AddSupport { tile_type, n } => {
                let c = match (
                    self.internal_conflict.as_mut(),
                    self.external_conflict.as_mut(),
                ) {
                    (Some(c), _) => c,
                    (_, Some(conflicts)) => {
                        let Some(c) = conflicts.conflicts.iter_mut().find(|c| c.leader.as_tile_type() == tile_type) else {
                            unreachable!();
                        };
                        c
                    }
                    _ => unreachable!(),
                };

                if c.attacker == current_player {
                    c.attacker_support += n;
                    c.attacker_sent_support = true;
                } else {
                    c.defender_support += n;
                    c.defender_sent_support = true;
                }

                match c.leader.as_tile_type() {
                    TileType::Red => curr_player.hand_red -= n,
                    TileType::Black => curr_player.hand_black -= n,
                    TileType::Green => curr_player.hand_green -= n,
                    TileType::Blue => curr_player.hand_blue -= n,
                    TileType::Empty => unreachable!(),
                }

                if c.all_sent() {
                    // TODO: resolve conflict
                    if c.is_internal {
                        let attacker_points = c.attacker_base_strength + c.attacker_support;
                        let defender_points = c.defender_base_strength + c.defender_support;

                        if attacker_points > defender_points {
                            // attacker wins, remove defender
                            let defender_cell = self.board.get(c.attacker_pos);
                            defender_cell.leader = Leader::None;
                            defender_cell.player = Player::None;
                            self.players
                                .get_mut(c.attacker)
                                .add_score(c.leader.as_tile_type());
                        } else {
                            // defender wins, remove attacker
                            let attacker_cell = self.board.get(c.attacker_pos);
                            attacker_cell.leader = Leader::None;
                            attacker_cell.player = Player::None;
                            self.players
                                .get_mut(c.defender)
                                .add_score(c.leader.as_tile_type());
                        }
                    } else {
                    }

                    // defender draws back to 6
                    let defender = self.players.get_mut(c.defender);
                    match self.bag.draw_to_6(defender) {
                        Some(_) => {
                            self.internal_conflict = None;
                            self.external_conflict = None;
                            next_state = GameState::Normal;
                        }
                        None => {
                            next_state = GameState::GameOver;
                        }
                    }
                } else {
                    let tile_type = if c.is_internal {
                        TileType::Red
                    } else {
                        c.leader.as_tile_type()
                    };
                    self.play_action_stack
                        .push((c.defender, PlayerAction::AddSupport(tile_type)));
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
                let mut external_conflicts = vec![];
                for leader in [Leader::Red, Leader::Black, Leader::Green, Leader::Blue] {
                    let leaders = kingdoms
                        .iter()
                        .filter_map(|k| k.get_leader_stats(leader))
                        .collect::<Vec<_>>();
                    if leaders.len() > 1 {
                        let (attacker, attacker_pos, attacker_base_strength) = leaders
                            .iter()
                            .find(|l| l.0 == current_player)
                            .copied()
                            .unwrap();
                        let (defender, defender_pos, defender_base_strength) = leaders
                            .iter()
                            .find(|l| l.0 != current_player)
                            .copied()
                            .unwrap();
                        external_conflicts.push(Conflict {
                            is_internal: false,
                            attacker,
                            defender,
                            attacker_pos,
                            defender_pos,
                            leader,
                            attacker_support: 0,
                            defender_support: 0,
                            attacker_sent_support: false,
                            defender_sent_support: false,
                            attacker_base_strength,
                            defender_base_strength,
                        });
                    }
                }

                if !external_conflicts.is_empty() {
                    // start a conflict
                    let red = external_conflicts.iter().any(|c| c.leader == Leader::Red);
                    let black = external_conflicts.iter().any(|c| c.leader == Leader::Black);
                    let green = external_conflicts.iter().any(|c| c.leader == Leader::Green);
                    let blue = external_conflicts.iter().any(|c| c.leader == Leader::Blue);

                    cell.terrain = Terrain::UnificationTile;
                    self.external_conflict = Some(ExternalConflict {
                        conflicts: external_conflicts,
                        unification_tile_pos: to,
                    });

                    // attacker can select which leader to resolve first
                    next_state = GameState::WarSelectLeader;
                    self.play_action_stack.push((
                        current_player,
                        PlayerAction::SelectLeader {
                            red,
                            black,
                            green,
                            blue,
                        },
                    ));
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
                        TileType::Empty => unreachable!(),
                    }
                    let scoring_player = kingdoms
                        .iter()
                        .filter_map(|k| k.get_leader_stats(tile_type.as_leader()).map(|p| p.0))
                        .next();
                    if let Some(p) = scoring_player {
                        self.players.get_mut(p).add_score(tile_type);
                    }
                }
            }
            Action::MoveLeader { movement, leader } => {
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
                                Leader::Black if kingdom.black_leader.is_some() => {
                                    kingdom.black_leader
                                }
                                Leader::Green if kingdom.green_leader.is_some() => {
                                    kingdom.green_leader
                                }
                                Leader::Blue if kingdom.blue_leader.is_some() => {
                                    kingdom.blue_leader
                                }
                                _ => None,
                            };
                        }

                        if let Some((player_2, p2_pos)) = has_conflict {
                            let attacker_base_strength =
                                pos.neighbors()
                                    .iter()
                                    .map(|n| self.board.get(*n).tile_type)
                                    .filter(|t| *t == TileType::Red)
                                    .count() as u8;
                            let defender_base_strength = p2_pos
                                .neighbors()
                                .iter()
                                .map(|n| self.board.get(*n).tile_type)
                                .filter(|t| *t == TileType::Red)
                                .count()
                                as u8;
                            self.internal_conflict = Some(Conflict {
                                is_internal: true,
                                attacker: current_player,
                                defender: player_2,
                                leader,
                                attacker_pos: pos,
                                attacker_base_strength,
                                defender_pos: p2_pos,
                                attacker_support: 0,
                                defender_support: 0,
                                defender_base_strength,
                                attacker_sent_support: false,
                                defender_sent_support: false,
                            });

                            next_state = GameState::AddSupport;
                            self.play_action_stack
                                .push((current_player, PlayerAction::AddSupport(TileType::Red)));
                        }

                        cell.leader = leader;
                        cell.player = current_player;
                        match leader {
                            Leader::Red => curr_player.has_red_leader = false,
                            Leader::Black => curr_player.has_black_leader = false,
                            Leader::Green => curr_player.has_green_leader = false,
                            Leader::Blue => curr_player.has_blue_leader = false,
                            _ => unreachable!(),
                        }
                    }
                    Movement::Move { from, to } => {
                        let from_cell = self.board.get(from);
                        let to_cell = self.board.get(to);
                        from_cell.leader = Leader::None;
                        to_cell.player = from_cell.player;
                        from_cell.player = Player::None;
                        to_cell.leader = leader;
                        // TODO: check internal conflict
                    }
                    Movement::Withdraw(pos) => {
                        let cell = &mut self.board.0[pos.x as usize][pos.y as usize];
                        match cell.leader {
                            Leader::Red => curr_player.has_red_leader = true,
                            Leader::Black => curr_player.has_black_leader = true,
                            Leader::Green => curr_player.has_green_leader = true,
                            Leader::Blue => curr_player.has_blue_leader = true,
                            _ => unreachable!(),
                        }
                        cell.leader = Leader::None;
                        cell.player = Player::None;
                    }
                }
            }
            Action::ReplaceTile(
                t @ Tiles {
                    red,
                    black,
                    green,
                    blue,
                },
            ) => {
                curr_player.hand_red -= red;
                curr_player.hand_black -= black;
                curr_player.hand_green -= green;
                curr_player.hand_blue -= blue;

                self.bag.player_draw(curr_player, t.sum());
            }
            Action::Pass => {}
        };

        // check if treasure is available for taking
        for kingdom in self.board.kingdoms() {
            if kingdom.treasures.len() > 1 && kingdom.green_leader.is_some() {
                let green_player = kingdom.green_leader.unwrap().0;
                let treasures = kingdom.treasures.map(Option::unwrap);
                self.play_action_stack
                    .push((green_player, PlayerAction::TakeTreasure(treasures)));
            }
        }

        // check if game is over
        if self.board.available_treasures_count() == 2 || self.bag.is_empty() {
            next_state = GameState::GameOver;
        }

        next_state
    }

    fn set_next_player_if_empty(&mut self) {
        if self.play_action_stack.is_empty() {
            let current_player = self.player_turn;
            let curr_player = self.players.get_mut(current_player);

            let opposite = match current_player {
                Player::None => unreachable!(),
                Player::Player1 => Player::Player2,
                Player::Player2 => Player::Player1,
            };
            match self.bag.draw_to_6(curr_player) {
                Some(()) => {
                    self.play_action_stack.push((opposite, PlayerAction::Any));
                    self.play_action_stack.push((opposite, PlayerAction::Any));
                    self.player_turn = opposite;
                }
                None => {
                    self.state = GameState::GameOver;
                }
            }
        }
    }

    fn neighboring_kingdoms(&self, to: Pos) -> Vec<Kingdom> {
        let mut kingdoms = vec![];
        let mut visited = [[false; W]; H];
        for neighbor in to.neighbors() {
            if visited[neighbor.x as usize][neighbor.y as usize] {
                continue;
            }
            let k = self.board.find_kingdom(neighbor, &mut visited);
            if k.has_leader() {
                kingdoms.push(k);
            }
        }
        kingdoms
    }

    pub fn process(&mut self, action: Action) -> Result<(Player, PlayerAction)> {
        let current_player = self.play_action_stack.iter().last().copied().unwrap().0;
        self.validate_action(action, current_player)?;
        let (current_player, _) = self.play_action_stack.pop().unwrap();
        self.state = self.process_action(action, current_player);
        self.set_next_player_if_empty();
        if self.state == GameState::GameOver {
            Err(Error::GameOver)
        } else {
            let ret = self.play_action_stack.iter().last().copied().unwrap();
            Ok(ret)
        }
    }

    #[must_use]
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
            if visited[pos.x as usize][pos.y as usize] {
                continue;
            }
            if pos == p2_pos {
                return true;
            }
            visited[pos.x as usize][pos.y as usize] = true;
            for neighbor in pos.neighbors() {
                if self.board.get(pos).is_connectable() {
                    queue.push(neighbor);
                }
            }
        }

        false
    }
}

#[derive(Copy, PartialEq, Eq, Debug, Clone)]
pub enum GameState {
    Normal,
    TakeTreasure,
    AddSupport,
    GameOver,
    WarSelectLeader,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum PlayerAction {
    AddSupport(TileType),
    SelectLeader {
        red: bool,
        blue: bool,
        green: bool,
        black: bool,
    },
    Any,
    TakeTreasure([Pos; 2]),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternalConflict {
    conflicts: Vec<Conflict>,
    unification_tile_pos: Pos,
}

/// occurs when placing same leaders in same kingdom
#[derive(Debug, Clone, Copy, Eq)]
pub struct Conflict {
    is_internal: bool,
    leader: Leader,

    attacker: Player,
    attacker_pos: Pos,
    attacker_support: u8,
    attacker_sent_support: bool,
    attacker_base_strength: u8,

    defender: Player,
    defender_pos: Pos,
    defender_support: u8,
    defender_sent_support: bool,
    defender_base_strength: u8,
}
impl Conflict {
    fn all_sent(&self) -> bool {
        self.attacker_sent_support && self.defender_sent_support
    }
}

impl PartialEq for Conflict {
    fn eq(&self, other: &Self) -> bool {
        // ignore support
        self.attacker == other.attacker
            && self.defender == other.defender
            && self.leader == other.leader
            && self.attacker_pos == other.attacker_pos
            && self.defender_pos == other.defender_pos
            && self.attacker_base_strength == other.attacker_base_strength
            && self.defender_base_strength == other.defender_base_strength
            && self.is_internal == other.is_internal
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    WarSelectLeader { leader: Leader },
    AddSupport { tile_type: TileType, n: u8 },
    PlaceTile { to: Pos, tile_type: TileType },
    TakeTreasure { pos: Pos },
    MoveLeader { movement: Movement, leader: Leader },
    ReplaceTile(Tiles),
    PlaceCatastrophe { to: Pos },
    Pass,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    #[must_use]
    fn draw_to_6(&mut self, player: &mut PlayerState) -> Option<()> {
        let n = 6 - player.hand_sum();
        self.player_draw(player, n)
    }

    fn player_draw(&mut self, player: &mut PlayerState, n: u8) -> Option<()> {
        let drawn = self.draw(n)?;
        player.hand_red += drawn.red;
        player.hand_black += drawn.black;
        player.hand_green += drawn.green;
        player.hand_blue += drawn.blue;
        Some(())
    }

    /// randomly draw tiles from the bag(counters for 4 colors) without replacement:
    fn draw(&mut self, n: u8) -> Option<Tiles> {
        // random number between 0 and the number of tiles left in the bag
        let mut tiles = Tiles {
            red: 0,
            black: 0,
            green: 0,
            blue: 0,
        };

        let mut n = n;
        while n > 0 {
            if self.is_empty() {
                return None;
            }
            let mut rand = rand::random::<u8>();
            rand %= self.sum();
            if rand < self.red {
                if self.red == 0 {
                    continue;
                }
                tiles.red += 1;
                self.red -= 1;
            } else if rand < self.red + self.black {
                if self.black == 0 {
                    continue;
                }
                tiles.black += 1;
                self.black -= 1;
            } else if rand < self.red + self.black + self.green {
                if self.green == 0 {
                    continue;
                }
                tiles.green += 1;
                self.green -= 1;
            } else {
                if self.blue == 0 {
                    continue;
                }
                tiles.blue += 1;
                self.blue -= 1;
            }

            n -= 1;
        }

        Some(tiles)
    }

    fn is_empty(&self) -> bool {
        self.red == 0 && self.black == 0 && self.green == 0 && self.blue == 0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Movement {
    Place(Pos),
    Move { from: Pos, to: Pos },
    Withdraw(Pos),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pos {
    pub x: u8,
    pub y: u8,
}

macro_rules! pos {
    ($x:expr, $y:expr) => {
        Pos { x: $x, y: $y }
    };
}

impl Pos {
    pub fn neighbors(&self) -> Vec<Pos> {
        let mut ret = Vec::new();
        if self.x > 0 {
            ret.push(pos!(self.x - 1, self.y));
        }
        if self.x < H as u8 - 1 {
            ret.push(pos!(self.x + 1, self.y));
        }
        if self.y > 0 {
            ret.push(pos!(self.x, self.y - 1));
        }
        if self.y < W as u8 - 1 {
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

            num_catastrophes: 2,
        }
    }

    /// get the score of the player
    /// which is the lowest score of the 4 colors
    /// treasure is the wildcard, add 1 treasure to lowest color, until we run out of treasure
    pub fn calculate_score(&self) -> u8 {
        let mut treasures = self.score_treasure;
        let mut current_lowest_color = TileType::Empty;
        let mut s = *self;

        while treasures > 0 {
            if s.score_green <= s.score_red
                && s.score_green <= s.score_black
                && s.score_green <= s.score_blue
            {
                current_lowest_color = TileType::Green;
            } else if s.score_black <= s.score_red
                && s.score_black <= s.score_green
                && s.score_black <= s.score_blue
            {
                current_lowest_color = TileType::Black;
            } else if s.score_red <= s.score_black
                && s.score_red <= s.score_green
                && s.score_red <= s.score_blue
            {
                current_lowest_color = TileType::Red;
            } else if s.score_blue <= s.score_black
                && s.score_blue <= s.score_green
                && s.score_blue <= s.score_red
            {
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

        s.score_black
            .min(s.score_red)
            .min(s.score_green)
            .min(s.score_blue)
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

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

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
            "..........t.....",
        ];
        for (x, row) in board.iter().enumerate() {
            for (y, c) in row.chars().enumerate() {
                match c {
                    'x' => ret.0[x][y].terrain = Terrain::River,
                    't' => {
                        ret.0[x][y].terrain = Terrain::Treasure;
                        ret.0[x][y].tile_type = TileType::Red;
                    }
                    '.' => {}
                    _ => unreachable!(),
                }
            }
        }

        ret
    }

    pub fn get(&self, pos: Pos) -> &mut Cell {
        unsafe { &mut *(&self.0[pos.x as usize][pos.y as usize] as *const Cell as *mut Cell) }
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
            treasures: [None; 2],
        };

        let mut stack = vec![pos];

        while let Some(pos) = stack.pop() {
            if visited[pos.x as usize][pos.y as usize] {
                continue;
            }
            visited[pos.x as usize][pos.y as usize] = true;

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
                    TileType::Empty => unreachable!(),
                }
            }

            if cell.leader != Leader::None {
                match cell.leader {
                    Leader::Red => kingdom.red_leader = Some((cell.player, pos)),
                    Leader::Black => kingdom.black_leader = Some((cell.player, pos)),
                    Leader::Green => kingdom.green_leader = Some((cell.player, pos)),
                    Leader::Blue => kingdom.blue_leader = Some((cell.player, pos)),
                    Leader::None => unreachable!(),
                }
            }

            if cell.terrain == Terrain::Treasure {
                match &mut kingdom.treasures {
                    [None, None] => kingdom.treasures[0] = Some(pos),
                    [Some(_), None] => kingdom.treasures[1] = Some(pos),
                    _ => unreachable!(),
                }
            }

            for neighbor_pos in pos.neighbors() {
                stack.push(neighbor_pos);
            }
        }

        kingdom
    }

    /// find all the kingdoms on the board
    fn kingdoms(&self) -> Vec<Kingdom> {
        let mut visited = [[false; W]; H];
        let mut kingdoms = vec![];
        for x in 0..H {
            for y in 0..W {
                if visited[x][y] {
                    continue;
                }
                let kingdom = self.find_kingdom(pos!(x as u8, y as u8), &mut visited);
                if kingdom.has_leader() {
                    kingdoms.push(kingdom);
                }
            }
        }
        kingdoms
    }

    fn available_treasures_count(&self) -> u8 {
        let mut c = 0;
        for x in 0..H {
            for y in 0..W {
                if self.0[x][y].terrain == Terrain::Treasure {
                    c += 1;
                }
            }
        }

        c
    }
}

#[derive(PackedStruct, Copy, Clone, Debug)]
#[packed_struct(bit_numbering = "msb0")]
pub struct Cell {
    #[packed_field(bits = "0..=2", ty = "enum")]
    tile_type: TileType,
    #[packed_field(bits = "3..=5", ty = "enum")]
    player: Player,
    #[packed_field(bits = "6..=8", ty = "enum")]
    leader: Leader,
    #[packed_field(bits = "9..=10", ty = "enum")]
    terrain: Terrain,
}

impl Cell {
    pub fn new() -> Self {
        Cell {
            tile_type: TileType::Empty,
            player: Player::None,
            leader: Leader::None,
            terrain: Terrain::Empty,
        }
    }

    fn is_connectable(&self) -> bool {
        self.tile_type != TileType::Empty
            || self.leader != Leader::None
            || self.terrain == Terrain::UnificationTile
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
    fn as_leader(&self) -> Leader {
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
    fn as_tile_type(&self) -> TileType {
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
    Empty,
    Treasure,
    River,
    Catastrophe,
    UnificationTile,
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
    InvalidAction(GameState),
    NoTreasure,
    GameOver,
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
        assert_eq!(game.play_action_stack, vec![p1!(), p1!()]);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
        let ret = game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 0)),
            leader: Leader::Black,
        });
        assert_eq!(
            ret.unwrap_err(),
            Error::CannotPlaceLeaderNotAdjacentToTemple
        );
    }

    #[test]
    fn leader_must_be_placed_next_to_temples_ok() {
        let mut game = Game::new();
        let ret = game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 1)),
            leader: Leader::Black,
        });
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Any));
        let cell = game.board.get(pos!(0, 1));
        assert_eq!(cell.leader, Leader::Black);
        assert_eq!(cell.player, Player::Player1);

        let ret = game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(1, 0)),
            leader: Leader::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Any));
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

        // player 1 puts down black and red leaders
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 1)),
            leader: Leader::Black,
        })
        .unwrap();
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(1, 0)),
            leader: Leader::Red,
        })
        .unwrap();

        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);

        // player 2 puts down red leader
        assert_eq!(game.play_action_stack, vec![p2!(), p2!()]);
        let ret = game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(2, 1)),
            leader: Leader::Red,
        });
        assert_eq!(
            ret.unwrap(),
            (Player::Player2, PlayerAction::AddSupport(TileType::Red))
        );
        assert!(game.internal_conflict.is_some());
        assert_eq!(
            game.internal_conflict.unwrap(),
            Conflict {
                is_internal: true,
                attacker: Player::Player2,
                defender: Player::Player1,
                leader: Leader::Red,
                attacker_pos: pos!(2, 1),
                defender_pos: pos!(1, 0),
                attacker_support: 0,
                defender_support: 0,
                attacker_sent_support: false,
                defender_sent_support: false,
                attacker_base_strength: 1,
                defender_base_strength: 1,
            }
        );
        assert_eq!(
            game.play_action_stack,
            vec![p2!(), p2!(PlayerAction::AddSupport(TileType::Red))]
        );

        // player 2 can only add red support, trying to do anything else is an error
        let ret = game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(2, 1)),
            leader: Leader::Red,
        });
        assert_eq!(
            ret.unwrap_err(),
            Error::InvalidAction(GameState::AddSupport)
        );

        // player 2 tries to add 6 support, but fails
        assert_eq!(
            game.play_action_stack,
            vec![p2!(), p2!(PlayerAction::AddSupport(TileType::Red))]
        );
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Black);
        let ret = game.process(Action::AddSupport {
            tile_type: TileType::Empty,
            n: 6,
        });
        assert_eq!(ret.unwrap_err(), Error::NotEnoughTiles);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);

        // player 2 successfully adds 1 support
        let ret = game.process(Action::AddSupport {
            tile_type: TileType::Empty,
            n: 1,
        });
        assert_eq!(
            game.play_action_stack,
            vec![p2!(), p1!(PlayerAction::AddSupport(TileType::Red))]
        );
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 5);
        assert_eq!(game.internal_conflict.unwrap().attacker_support, 1);
        assert_eq!(game.internal_conflict.unwrap().defender_support, 0);
        assert!(game.internal_conflict.unwrap().attacker_sent_support);
        assert!(!game.internal_conflict.unwrap().defender_sent_support);
        assert_eq!(
            ret.unwrap(),
            (Player::Player1, PlayerAction::AddSupport(TileType::Red))
        );

        // player 1 successfully adds 1 support
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        let ret = game.process(Action::AddSupport {
            tile_type: TileType::Empty,
            n: 1,
        });
        assert_eq!(game.play_action_stack, vec![p2!()]);
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Any));
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 5);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6); // defender draws

        let ret = game.process(Action::Pass);
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Any));
        assert_eq!(game.play_action_stack, vec![p1!(), p1!()]);

        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
    }

    #[test]
    fn score_is_correct() {
        let mut player = PlayerState {
            score_black: 1,
            score_blue: 2,
            score_green: 3,
            score_red: 4,
            ..Default::default()
        };
        assert_eq!(player.calculate_score(), 1);
        player.score_treasure = 1;
        assert_eq!(player.calculate_score(), 2);
        player.score_treasure = 3;
        assert_eq!(player.calculate_score(), 3);
    }

    #[test]
    fn place_tile() {
        let mut game = Game::new();
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 1)),
            leader: Leader::Red,
        })
        .unwrap();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        let ret = game.process(Action::PlaceTile {
            to: pos!(0, 0),
            tile_type: TileType::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Any));
        assert_eq!(game.board.get(pos!(0, 0)).tile_type, TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 1);
    }

    #[test]
    fn place_tile_no_leader() {
        let mut game = Game::new();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        let ret = game.process(Action::PlaceTile {
            to: pos!(0, 0),
            tile_type: TileType::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Any));
        assert_eq!(game.board.get(pos!(0, 0)).tile_type, TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 5);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 0);
    }

    #[test]
    fn place_tile_score_enemy_leader() {
        let mut game = Game::new();
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 1)),
            leader: Leader::Red,
        })
        .unwrap();
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(1, 0)),
            leader: Leader::Black,
        })
        .unwrap();
        assert_eq!(game.play_action_stack, vec![p2!(), p2!()]);
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        game.process(Action::PlaceTile {
            to: pos!(0, 0),
            tile_type: TileType::Red,
        })
        .unwrap();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Black);
        game.process(Action::PlaceTile {
            to: pos!(2, 0),
            tile_type: TileType::Black,
        })
        .unwrap();
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);

        assert_eq!(game.players.get_mut(Player::Player1).score_red, 1);
        assert_eq!(game.players.get_mut(Player::Player1).score_black, 1);
    }

    #[test]
    fn place_tile_trigger_external_conflict() {
        let mut game = Game::new();

        // player 1 place a leader and a tile
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(1, 0)),
            leader: Leader::Red,
        })
        .unwrap();
        game.process(Action::PlaceTile {
            to: pos!(0, 2),
            tile_type: TileType::Red,
        })
        .unwrap();
        assert_eq!(game.board.get(pos!(0, 2)).tile_type, TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 0);

        // player 2 places a leader
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 3)),
            leader: Leader::Red,
        })
        .unwrap();

        // player 2 places a tile joining two kingdoms
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        let ret = game
            .process(Action::PlaceTile {
                to: pos!(0, 1),
                tile_type: TileType::Red,
            })
            .unwrap();
        assert_eq!(
            game.play_action_stack,
            vec![p2!(PlayerAction::SelectLeader {
                red: true,
                blue: false,
                green: false,
                black: false
            })]
        );
        assert_eq!(
            ret,
            (
                Player::Player2,
                PlayerAction::SelectLeader {
                    red: true,
                    blue: false,
                    green: false,
                    black: false
                }
            )
        );
        assert_eq!(
            game.external_conflict,
            Some(ExternalConflict {
                conflicts: vec![Conflict {
                    is_internal: false,
                    attacker: Player::Player2,
                    defender: Player::Player1,
                    leader: Leader::Red,
                    attacker_pos: pos!(0, 3),
                    defender_pos: pos!(1, 0),
                    attacker_support: 0,
                    defender_support: 0,
                    attacker_sent_support: false,
                    defender_sent_support: false,
                    attacker_base_strength: 1,
                    defender_base_strength: 1,
                }],
                unification_tile_pos: pos!(0, 1)
            })
        );

        // player 2 (attacker) selects leader
        game.process(Action::WarSelectLeader {
            leader: Leader::Red,
        })
        .unwrap();
        assert_eq!(
            game.play_action_stack,
            vec![(Player::Player2, PlayerAction::AddSupport(TileType::Red))]
        );

        // adds support
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        game.process(Action::AddSupport {
            tile_type: TileType::Red,
            n: 1,
        })
        .unwrap();
        assert_eq!(
            game.external_conflict.as_ref().unwrap().conflicts[0].attacker_support,
            1
        );
        assert_eq!(
            game.external_conflict.as_ref().unwrap().conflicts[0].defender_support,
            0
        );
        assert_eq!(
            game.external_conflict.as_ref().unwrap().conflicts[0].attacker_base_strength,
            1
        );
        assert_eq!(
            game.external_conflict.as_ref().unwrap().conflicts[0].defender_base_strength,
            1
        );
        assert_eq!(
            game.play_action_stack,
            vec![(Player::Player1, PlayerAction::AddSupport(TileType::Red))]
        );

        // player 1 (defender) adds support
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        let ret = game
            .process(Action::AddSupport {
                tile_type: TileType::Red,
                n: 1,
            })
            .unwrap();
        assert_eq!(game.play_action_stack, vec![p1!(), p1!()]);
        assert_eq!(ret, (Player::Player1, PlayerAction::Any));
    }

    #[test]
    fn place_tile_does_not_trigger_external_conflict() {
        let mut game = Game::new();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(1, 0)),
            leader: Leader::Red,
        })
        .unwrap();
        game.process(Action::PlaceTile {
            to: pos!(0, 2),
            tile_type: TileType::Red,
        })
        .unwrap();
        assert_eq!(game.board.get(pos!(0, 2)).tile_type, TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 0);
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 3)),
            leader: Leader::Black,
        })
        .unwrap();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        let ret = game
            .process(Action::PlaceTile {
                to: pos!(0, 1),
                tile_type: TileType::Red,
            })
            .unwrap();
        assert_eq!(ret, (Player::Player1, PlayerAction::Any));
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 1);
        assert_eq!(game.players.get_mut(Player::Player2).score_red, 0);
    }
}
