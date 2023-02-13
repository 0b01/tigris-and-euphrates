use packed_struct::prelude::*;

pub const W: usize = 16;
pub const H: usize = 11;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Players(pub [PlayerState; 2]);
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

#[derive(Clone, Eq, PartialEq)]
pub struct TnEGame {
    pub board: Board,
    pub players: Players,
    bag: Tiles,

    pub state: Vec<GameState>,

    pub play_action_stack: Vec<(Player, PlayerAction)>,
    player_turn: Player,
    pub last_player: Player,

    /// Revolt
    pub internal_conflict: Option<Conflict>,

    /// War
    pub external_conflict: Option<ExternalConflict>,
}

impl std::fmt::Debug for TnEGame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TnE")
            // .field("board", &self.board)
            .field("players", &self.players)
            .field("bag", &self.bag)
            .field("state", &self.state)
            .field("play_action_stack", &self.play_action_stack)
            .field("player_turn", &self.player_turn)
            .field("last_player", &self.last_player)
            .field("internal_conflict", &self.internal_conflict)
            .field("external_conflict", &self.external_conflict).finish()
    }
}

pub struct Kingdom {
    pub red_leader: Option<(Player, Pos)>,
    pub black_leader: Option<(Player, Pos)>,
    pub green_leader: Option<(Player, Pos)>,
    pub blue_leader: Option<(Player, Pos)>,

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

    pub fn get_leader_info(&self, leader: Leader) -> Option<(Player, Pos, u8)> {
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
        (Player::Player1, PlayerAction::Normal)
    };
    ($i:expr) => {
        (Player::Player1, $i)
    };
}

#[allow(unused)]
macro_rules! p2 {
    () => {
        (Player::Player2, PlayerAction::Normal)
    };
    ($i:expr) => {
        (Player::Player2, $i)
    };
}

impl Default for TnEGame {
    fn default() -> Self {
        Self::new()
    }
}

impl TnEGame {
    pub fn new() -> Self {
        let mut game = TnEGame {
            board: Board::new(),
            players: Players([PlayerState::new(); 2]),
            state: vec![GameState::Normal],
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
            last_player: Player::None,
        };

        // initial draw for players, each player draws 6 tiles
        for player in &mut game.players.0 {
            game.bag.player_draw(player, 6);
        }

        game
    }

    pub fn validate_action(&self, action: Action, state: GameState) -> Result<()> {
        let current_player = self.next_player();
        let curr_player_state = self.players.get_mut(current_player);

        let is_valid = match (state, action) {
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
            return Err(Error::InvalidAction(state, action));
        }

        match action {
            Action::TakeTreasure(pos) => {
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
                    dbg!('a');
                    return Err(Error::LeadersDisconnected);
                }

                // the player must be the right one to send support
                if conflict.attacker_sent_support && conflict.attacker == current_player {
                    return Err(Error::AlreadySentSupport);
                }

                // the player must have the tile they are trying to add support with
                if curr_player_state.get_hand(tile_type) < n {
                    return Err(Error::NotEnoughTiles);
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
                    dbg!('b');
                    return Err(Error::LeadersDisconnected);
                }
            }
            Action::PlaceCatastrophe { to } => {
                if curr_player_state.num_catastrophes == 0 {
                    return Err(Error::NoCatastrophes);
                }
                // cannot be placed if the destination contains a leader
                let cell = self.board.get(to);
                if cell.leader != Leader::None {
                    return Err(Error::CannotPlaceOverLeader);
                }
                // cannot place over unclaimed treasure
                if cell.terrain == Terrain::Treasure {
                    return Err(Error::CannotPlaceOverTreasure);
                }
            }
            Action::PlaceTile { to, tile_type } => {
                let cell = self.board.get(to);
                if cell.tile_type != TileType::Empty {
                    return Err(Error::CannotPlaceTileOverTile);
                }
                if cell.leader != Leader::None {
                    return Err(Error::CannotPlaceOverLeader);
                }
                if curr_player_state.get_hand(tile_type) == 0 {
                    return Err(Error::NotEnoughTiles);
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
                if self.board.neighboring_kingdoms(to).len() as u8 > 2 {
                    return Err(Error::CannotJoinThreeKingdoms);
                }
            }
            Action::MoveLeader {
                movement: ty,
                leader,
            } => match ty {
                Movement::Place(to) => {
                    if curr_player_state.get_leader(leader).is_some() {
                        return Err(Error::LeaderAlreadyPlaced);
                    }
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
                if curr_player_state.hand_red < red
                    || curr_player_state.hand_black < black
                    || curr_player_state.hand_green < green
                    || curr_player_state.hand_blue < blue
                {
                    return Err(Error::NotEnoughTiles);
                }
            }
            Action::Pass => {}
        }

        Ok(())
    }

    pub fn process_action(&mut self, action: Action, current_player: Player) -> Option<GameState> {
        let curr_player = self.players.get_mut(current_player);

        let next_state = match action {
            Action::TakeTreasure(pos) => {
                let cell = self.board.get(pos);
                cell.terrain = Terrain::Empty;

                curr_player.score_treasure += 1;
                None
            }
            Action::WarSelectLeader { leader } => {
                assert!(self.internal_conflict.is_none());
                let Some(conflicts) = &mut self.external_conflict else {
                    unreachable!();
                };
                let Some(conflict) = conflicts.conflicts.iter_mut().find(|c| c.leader == leader) else {
                    unreachable!();
                };

                let next_action = PlayerAction::AddSupport(conflict.leader.as_tile_type());
                self.play_action_stack
                    .push((conflict.attacker, next_action));
                Some(GameState::AddSupport)
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
                let attacker = c.attacker;
                let defender = c.defender;

                if attacker == current_player {
                    c.attacker_support += n;
                    c.attacker_sent_support = true;
                } else {
                    c.defender_support += n;
                    c.defender_sent_support = true;
                }

                match tile_type {
                    TileType::Red => curr_player.hand_red -= n,
                    TileType::Black => curr_player.hand_black -= n,
                    TileType::Green => curr_player.hand_green -= n,
                    TileType::Blue => curr_player.hand_blue -= n,
                    TileType::Empty => unreachable!(),
                }

                if c.all_sent() {
                    if c.is_internal {
                        // internal conflict
                        let attacker_points = c.attacker_base_strength + c.attacker_support;
                        let defender_points = c.defender_base_strength + c.defender_support;

                        let (winner, loser, loser_pos) = if attacker_points > defender_points {
                            (attacker, defender, c.defender_pos)
                        } else {
                            (defender, attacker, c.attacker_pos)
                        };

                        // remove loser
                        self.players
                            .get_mut(loser)
                            .set_leader(c.leader, None);
                        let loser_cell = self.board.get(loser_pos);
                        loser_cell.leader = Leader::None;
                        loser_cell.player = Player::None;
                        // give winner 1 point
                        self.players
                            .get_mut(winner)
                            .add_score(c.leader.as_tile_type());
                    } else {
                        // external conflict
                        let attacker_points = c.attacker_base_strength + c.attacker_support;
                        let defender_points = c.defender_base_strength + c.defender_support;

                        let (winner, loser, loser_pos) = if attacker_points > defender_points {
                            (attacker, defender, c.defender_pos)
                        } else {
                            (defender, attacker, c.attacker_pos)
                        };

                        // remove loser
                        self.players
                            .get_mut(loser)
                            .set_leader(c.leader, None);
                        let loser_cell = self.board.get(loser_pos);
                        loser_cell.leader = Leader::None;
                        loser_cell.player = Player::None;
                        // give winner 1 point
                        self.players
                            .get_mut(winner)
                            .add_score(c.leader.as_tile_type());

                        // TODO: disintegrate kingdom and credit those points to the winner
                        let leader = c.leader;
                        drop(c);
                        self.external_conflict.as_mut().unwrap().conflicts.retain(|con| con.leader != leader);

                        // credit unification cell
                        let unification_tile_pos = self.external_conflict.as_ref().unwrap().unification_tile_pos;
                        let unification_tile_type = self.external_conflict.as_ref().unwrap().unification_tile_type;
                        let unification_cell = self.board.get(unification_tile_pos);
                        // revert to original
                        unification_cell.terrain = Board::lookup_terrain(unification_tile_pos);
                        unification_cell.tile_type = unification_tile_type;
                        let mut visited = [[false; W]; H];
                        let kingdom = self.board.find_kingdom(unification_tile_pos, &mut visited);
                        if let Some((p, _, _)) = kingdom.get_leader_info(unification_cell.tile_type.as_leader()) {
                            self.players.get_mut(p).add_score(unification_cell.tile_type);
                        }
                    }

                    // defender draws back to 6
                    let defender = self.players.get_mut(defender);
                    match self.bag.draw_to_6(defender) {
                        Some(_) => {
                            self.internal_conflict = None;
                        }
                        None => {
                            return None
                        }
                    };

                    // if external conflict, check if there's another leader we can select
                    if let Some(conflicts) = &mut self.external_conflict {
                        if conflicts.conflicts.is_empty() {
                            self.external_conflict = None;
                            Some(GameState::Normal)
                        } else {
                            let red = conflicts.conflicts.iter().any(|c| c.leader == Leader::Red);
                            let black = conflicts.conflicts.iter().any(|c| c.leader == Leader::Black);
                            let green = conflicts.conflicts.iter().any(|c| c.leader == Leader::Green);
                            let blue = conflicts.conflicts.iter().any(|c| c.leader == Leader::Blue);
                            self.play_action_stack.push((attacker, PlayerAction::SelectLeader {
                                red, black, green, blue
                            }));
                            Some(GameState::WarSelectLeader)
                        }
                    } else {
                        Some(GameState::Normal)
                    }
                } else {
                    let tile_type = if c.is_internal {
                        TileType::Red
                    } else {
                        c.leader.as_tile_type()
                    };
                    self.play_action_stack
                        .push((c.defender, PlayerAction::AddSupport(tile_type)));
                    Some(GameState::AddSupport)
                }
            }
            Action::PlaceCatastrophe { to } => {
                let cell = self.board.get(to);
                cell.terrain = Terrain::Catastrophe;
                cell.leader = Leader::None;
                cell.player = Player::None;
                cell.tile_type = TileType::Empty;

                curr_player.num_catastrophes -= 1;
                Some(GameState::Normal)
            }
            Action::PlaceTile { to, tile_type } => {
                let cell = self.board.get(to);
                // get kingdoms around the to-be-placed tile
                let kingdoms = self.board.neighboring_kingdoms(to);
                // 1. find if there's an external conflict, if it would unite kingdoms with same color leaders
                let mut external_conflicts = vec![];
                for leader in [Leader::Red, Leader::Black, Leader::Green, Leader::Blue] {
                    let leaders = kingdoms
                        .iter()
                        .filter_map(|k| k.get_leader_info(leader))
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
                        unification_tile_type: tile_type,
                    });

                    // attacker can select which leader to resolve first
                    self.play_action_stack.push((
                        current_player,
                        PlayerAction::SelectLeader {
                            red,
                            black,
                            green,
                            blue,
                        },
                    ));
                    Some(GameState::WarSelectLeader)
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
                    let matching_leader = kingdoms
                        .iter()
                        .find_map(|k| k.get_leader_info(tile_type.as_leader())
                        .map(|(p,_,_)| p));

                    // if the matching leader is not in the kingdom, check if black leader is in the kingdom
                    // and if so, score for black leader
                    let black_leader = kingdoms
                        .iter()
                        .find_map(|k| k.get_leader_info(Leader::Black)
                        .map(|(p,_,_)| p));

                    match (matching_leader, black_leader) {
                        (Some(p), _) |
                        (None, Some(p)) => self.players.get_mut(p).add_score(tile_type),
                        (None, None) => (),
                    }
                    Some(GameState::Normal)
                }
            }
            Action::MoveLeader { movement, leader } => {
                match movement {
                    Movement::Place(pos) => {
                        let next_state = self.check_internal_conflict(pos, leader, current_player);

                        let cell = self.board.get(pos);
                        cell.leader = leader;
                        cell.player = current_player;
                        self.players.get_mut(current_player).set_leader(leader, Some(pos));
                        Some(next_state)
                    }
                    Movement::Move { from, to } => {
                        let next_state = self.check_internal_conflict(to, leader, current_player);

                        self.players.get_mut(current_player).set_leader(leader, Some(to));
                        let from_cell = self.board.get(from);
                        let to_cell = self.board.get(to);
                        from_cell.leader = Leader::None;
                        to_cell.player = from_cell.player;
                        from_cell.player = Player::None;
                        to_cell.leader = leader;
                        Some(next_state)
                    }
                    Movement::Withdraw(pos) => {
                        let cell = &mut self.board.0[pos.x as usize][pos.y as usize];
                        self.players.get_mut(current_player).set_leader(cell.leader, None);
                        cell.leader = Leader::None;
                        cell.player = Player::None;
                        Some(GameState::Normal)
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
                Some(GameState::Normal)
            }
            Action::Pass => {
                Some(GameState::Normal)
            }
        };

        // check if game is over
        if self.board.available_treasures_count() == 2 || self.bag.is_empty() {
            None
        } else {
            next_state
        }
    }

    fn check_internal_conflict(&mut self, pos: Pos, leader: Leader, current_player: Player) -> GameState {
        // check internal conflict
        // if the kingdom already contains a leader, then an internal conflict is triggered
        let kingdoms = self.board.neighboring_kingdoms(pos);

        let mut has_conflict = None;
        for kingdom in kingdoms {
            has_conflict = match leader {
                Leader::Red if kingdom.red_leader.is_some() && kingdom.red_leader.unwrap().0 != current_player => kingdom.red_leader,
                Leader::Black if kingdom.black_leader.is_some() && kingdom.black_leader.unwrap().0 != current_player => kingdom.black_leader,
                Leader::Green if kingdom.green_leader.is_some() && kingdom.green_leader.unwrap().0 != current_player => kingdom.green_leader,
                Leader::Blue if kingdom.blue_leader.is_some() && kingdom.blue_leader.unwrap().0 != current_player => kingdom.blue_leader,
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

            self.play_action_stack
                .push((current_player, PlayerAction::AddSupport(TileType::Red)));
            GameState::AddSupport
        } else {
            GameState::Normal
        }
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

            self.play_action_stack.push((opposite, PlayerAction::Normal));
            self.play_action_stack.push((opposite, PlayerAction::Normal));
            self.player_turn = opposite;

            if self.bag.draw_to_6(&mut self.players.0[0]).is_none() { self.state.clear(); }
            if self.bag.draw_to_6(&mut self.players.0[1]).is_none() { self.state.clear(); }
        }
    }

    pub fn process(&mut self, action: Action) -> Result<(Player, PlayerAction)> {
        let current_state = self.state.pop().ok_or(Error::GameOver)?;
        if let Err(e) = self.validate_action(action, current_state) {
            self.state.push(current_state);
            return Err(e);
        }
        let (current_player, _) = self.play_action_stack.pop().unwrap();
        let next_state = self.process_action(action, current_player);
        // println!("{:?} => {:?}", self.state, next_state);
        if let Some(next_state) = next_state {
            self.state.push(next_state);
        }
        self.set_next_player_if_empty();

        // check if treasure is available for taking
        // TODO: must take corner treasures first
        for kingdom in self.board.kingdoms() {
            if kingdom.treasures.iter().filter(|i|i.is_some()).count() > 1 && kingdom.green_leader.is_some() {
                let green_player = kingdom.green_leader.unwrap().0;
                let treasures = kingdom.treasures.map(Option::unwrap);
                self.play_action_stack
                    .push((green_player, PlayerAction::TakeTreasure(treasures)));
                self.state.push(GameState::TakeTreasure);
            }
        }

        self.last_player = current_player;
        Ok(self.next())
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
        if self.board.nearby_kingdoms_count(pos) > 1 {
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

    pub fn winner(&self) -> Player {
        let a = self.players.0[0].calculate_score();
        let b = self.players.0[1].calculate_score();

        if a > b {
            Player::Player1
        } else if b > a {
            Player::Player2
        } else {
            Player::None
        }
    }

    pub fn next_action(&self) -> PlayerAction {
        self.play_action_stack.last().map(|(_, a)| *a).unwrap()
    }

    pub fn next_player(&self) -> Player {
        self.play_action_stack.last().map(|(p, _)| *p).unwrap()
    }

    pub fn next(&self) -> (Player, PlayerAction) {
        self.play_action_stack.last().copied().unwrap()
    }

    pub fn next_state(&self) -> GameState {
        self.state.last().copied().unwrap()
    }
}

#[derive(Copy, PartialEq, Eq, Debug, Clone)]
pub enum GameState {
    Normal,
    TakeTreasure,
    AddSupport,
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
    Normal,
    TakeTreasure([Pos; 2]),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternalConflict {
    pub conflicts: Vec<Conflict>,
    pub unification_tile_pos: Pos,
    pub unification_tile_type: TileType,
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
    TakeTreasure(Pos),
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub x: u8,
    pub y: u8,
}

impl std::fmt::Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let col = (self.y as u8 + 'A' as u8) as char;
        let row = self.x+1;
        write!(f, "{}{}", row, col)
    }
}

#[macro_export]
macro_rules! pos {
    // compile time convert 1A to 00
    ($s:expr) => {
        pos($s)
    };

    ($x:expr, $y:expr) => {
        Pos { x: $x, y: $y }
    };
}

pub const fn pos(a: &'static str) -> Pos {
    let chars = a.as_bytes();
    let x = chars[0] as u8 - '1' as u8;
    let y = chars[1] as u8 - 'A' as u8;
    Pos { x, y }
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
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct PlayerState {
    pub placed_red_leader: Option<Pos>,
    pub placed_black_leader: Option<Pos>,
    pub placed_green_leader: Option<Pos>,
    pub placed_blue_leader: Option<Pos>,

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
            placed_black_leader: None,
            placed_red_leader: None,
            placed_green_leader: None,
            placed_blue_leader: None,

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

    fn add_score_by(&mut self, tile_type: TileType, n: u8) {
        match tile_type {
            TileType::Green => self.score_green += n,
            TileType::Black => self.score_black += n,
            TileType::Red => self.score_red += n,
            TileType::Blue => self.score_blue += n,
            _ => (),
        }
    }

    fn add_score(&mut self, tile_type: TileType) {
        self.add_score_by(tile_type, 1)
    }

    fn set_leader(&mut self, leader: Leader, arg: Option<Pos>) {
        match leader {
            Leader::Red => self.placed_red_leader = arg,
            Leader::Black => self.placed_black_leader = arg,
            Leader::Green => self.placed_green_leader = arg,
            Leader::Blue => self.placed_blue_leader = arg,
            _ => unreachable!(),
        }
    }

    pub(crate) fn get_hand(&self, tile_type: TileType) -> u8 {
        match tile_type {
            TileType::Blue => self.hand_blue,
            TileType::Green => self.hand_green,
            TileType::Red => self.hand_red,
            TileType::Black => self.hand_black,
            TileType::Empty => unreachable!(),
        }
    }

    pub fn get_leader(&self, leader: Leader) -> Option<Pos> {
        match leader {
            Leader::None => unreachable!(),
            Leader::Blue => self.placed_blue_leader,
            Leader::Green => self.placed_green_leader,
            Leader::Red => self.placed_red_leader,
            Leader::Black => self.placed_black_leader,
        }
    }

    pub(crate) fn score_sum(&self) -> u8 {
        self.score_black + self.score_red + self.score_green + self.score_blue
    }

    pub fn get_eval(&self, state: &TnEGame) -> i16 {
        let mut s = 0;

        // 10 points for each final score
        s += self.calculate_score() as i16 * 10;

        // 5 points for kingdom size
        for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].into_iter() {
            if let Some(pos) = self.get_leader(leader) {
                let mut visited = [[false; W]; H];
                let kingdom = state.board.find_kingdom(pos, &mut visited);
                s += kingdom.get_leader_info(leader).unwrap().2 as i16 * 5;
            }
        }

        // 1 point for each point
        s += self.score_sum() as i16;

        s
    }

    pub(crate) fn hand_to_vec(&self) -> Vec<TileType> {
        let mut ret = Vec::new();
        for _ in 0..self.hand_black {
            ret.push(TileType::Black);
        }
        for _ in 0..self.hand_red {
            ret.push(TileType::Red);
        }
        for _ in 0..self.hand_green {
            ret.push(TileType::Green);
        }
        for _ in 0..self.hand_blue {
            ret.push(TileType::Blue);
        }
        ret
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Board(pub [[Cell; W]; H]);

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

const BOARD: [&str; 11] = [
    "....xxxxx.t.x...",
    ".t..x.......x..t",
    "...xxt......xx..",
    "xxxx.........xxx",
    ".............txx",
    "..............x.",
    "xxxx.....t..xxx.",
    ".t.xxxxx....x...",
    ".......xxxxxx.t.",
    "......t........",
    "..........t.....",
];

impl Board {
    /// Create a new board
    pub fn new() -> Self {
        let mut ret = Board([[Cell::new(); W]; H]);
        
        for (x, row) in BOARD.iter().enumerate() {
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

    pub fn neighboring_kingdoms(&self, to: Pos) -> Vec<Kingdom> {
        let mut kingdoms = vec![];
        let mut visited = [[false; W]; H];
        for neighbor in to.neighbors() {
            if visited[neighbor.x as usize][neighbor.y as usize] {
                continue;
            }
            let k = self.find_kingdom(neighbor, &mut visited);
            if k.has_leader() {
                kingdoms.push(k);
            }
        }
        kingdoms
    }

    #[must_use]
    fn nearby_kingdoms_count(&self, pos: Pos) -> u8 {
        self.neighboring_kingdoms(pos).len() as u8
    }

    pub fn find_empty_leader_space_next_to_red(&self) -> Vec<Pos> {
        let mut ret = Vec::new();
        for x in 0..H {
            for y in 0..W {
                if self.0[x][y].tile_type == TileType::Red {
                    for pos in pos!(x as u8, y as u8).neighbors() {
                        if self.nearby_kingdoms_count(pos) > 1 {
                            continue;
                        }

                        let cell = self.get(pos);
                        if cell.tile_type == TileType::Empty
                        && cell.terrain != Terrain::River
                        && cell.terrain != Terrain::Catastrophe
                        && cell.leader == Leader::None {
                            ret.push(pos);
                        }
                    }
                }
            }
        }

        ret
    }

    pub fn find_catastrophe_positions(&self) -> Vec<Pos> {
        let mut ret = Vec::new();
        for x in 0..H {
            for y in 0..W {
                let cell = self.0[x][y];
                if cell.leader == Leader::None && cell.tile_type != TileType::Empty && cell.terrain != Terrain::Treasure {
                    ret.push(pos!(x as u8, y as u8));
                }
            }
        }

        ret
    }

    pub fn find_kingdom(&self, pos: Pos, visited: &mut [[bool; W]; H]) -> Kingdom {
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
                    [Some(_), Some(_)] => (), // just ignore if kingdom has 3 treasures
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
    pub fn kingdoms(&self) -> Vec<Kingdom> {
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

    pub fn find_empty_spaces_adj_kingdom(&self, pos: Pos, is_river_space: bool) -> Vec<Pos> {
        let mut ret = Vec::new();
        let mut kingdom = [[false; W]; H];

        self.find_kingdom_map(pos, &mut kingdom);

        // find empty spaces around kingdom
        for x in 0..H {
            for y in 0..W {
                if kingdom[x][y] {
                    for pos in pos!(x as u8, y as u8).neighbors() {
                        let cell = self.get(pos);
                        if cell.tile_type == TileType::Empty
                        && cell.leader == Leader::None {
                            if is_river_space {
                                if cell.terrain == Terrain::River {
                                    ret.push(pos);
                                }
                            } else {
                                if cell.terrain != Terrain::River {
                                    ret.push(pos);
                                }
                            }
                        }
                    }
                }
            }
        }

        ret
    }

    pub fn find_kingdom_map(&self, start: Pos, kingdom: &mut [[bool; 16]; 11]) {
        let mut stack = vec![start];
        // if it's connectable, mark as kingdom
        while let Some(pos) = stack.pop() {
            if kingdom[pos.x as usize][pos.y as usize] {
                continue;
            }

            let cell = self.get(pos);
            if !cell.is_connectable() {
                continue;
            }

            kingdom[pos.x as usize][pos.y as usize] = true;

            for neighbor_pos in pos.neighbors() {
                stack.push(neighbor_pos);
            }
        }
    }

    fn lookup_terrain(unification_tile_pos: Pos) -> Terrain {
        let mut chars = BOARD[unification_tile_pos.x as usize].chars();
        let char = chars
            .nth(unification_tile_pos.y as usize)
            .unwrap();
        match char {
            'x' => Terrain::River,
            _ => Terrain::Empty,
        }
    }
}

#[derive(PackedStruct, Copy, Clone, Debug, Eq, PartialEq)]
#[packed_struct(bit_numbering = "msb0")]
pub struct Cell {
    #[packed_field(bits = "0..=2", ty = "enum")]
    pub tile_type: TileType,
    #[packed_field(bits = "3..=5", ty = "enum")]
    pub player: Player,
    #[packed_field(bits = "6..=8", ty = "enum")]
    pub leader: Leader,
    #[packed_field(bits = "9..=10", ty = "enum")]
    pub terrain: Terrain,
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
    pub fn as_tile_type(&self) -> TileType {
        match self {
            Leader::Blue => TileType::Blue,
            Leader::Green => TileType::Green,
            Leader::Red => TileType::Red,
            Leader::Black => TileType::Black,
            Leader::None => TileType::Empty,
        }
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq, Eq)]
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
    InvalidAction(GameState, Action),
    NoTreasure,
    GameOver,
    CannotPlaceOverTreasure,
    LeaderAlreadyPlaced,
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
        let mut game = TnEGame::new();
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
        let mut game = TnEGame::new();
        let ret = game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(0, 1)),
            leader: Leader::Black,
        });
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Normal));
        let cell = game.board.get(pos!(0, 1));
        assert_eq!(cell.leader, Leader::Black);
        assert_eq!(cell.player, Player::Player1);

        let ret = game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(1, 0)),
            leader: Leader::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Normal));
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
        let mut game = TnEGame::new();

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
        game.process(Action::MoveLeader {
            movement: Movement::Place(pos!(2, 1)),
            leader: Leader::Red,
        }).unwrap_err();

        // player 2 tries to add 6 support, but fails
        assert_eq!(
            game.play_action_stack,
            vec![p2!(), p2!(PlayerAction::AddSupport(TileType::Red))]
        );
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Black);
        let ret = game.process(Action::AddSupport {
            tile_type: TileType::Red,
            n: 6,
        });
        assert_eq!(ret.unwrap_err(), Error::NotEnoughTiles);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);

        // player 2 successfully adds 1 support
        let ret = game.process(Action::AddSupport {
            tile_type: TileType::Red,
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
            tile_type: TileType::Red,
            n: 1,
        });
        assert_eq!(game.play_action_stack, vec![p2!()]);
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Normal));
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 5);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6); // defender draws

        let ret = game.process(Action::Pass);
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Normal));
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
        let mut game = TnEGame::new();
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
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Normal));
        assert_eq!(game.board.get(pos!(0, 0)).tile_type, TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 1);
    }

    #[test]
    fn place_tile_no_leader() {
        let mut game = TnEGame::new();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        let ret = game.process(Action::PlaceTile {
            to: pos!(0, 0),
            tile_type: TileType::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Normal));
        assert_eq!(game.board.get(pos!(0, 0)).tile_type, TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 5);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 0);
    }

    #[test]
    fn place_tile_score_enemy_leader() {
        let mut game = TnEGame::new();
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
        let mut game = TnEGame::new();

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
                unification_tile_pos: pos!(0, 1),
                unification_tile_type: TileType::Red,
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
        assert_eq!(ret, (Player::Player1, PlayerAction::Normal));
    }

    #[test]
    fn place_tile_does_not_trigger_external_conflict() {
        let mut game = TnEGame::new();
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
        assert_eq!(ret, (Player::Player1, PlayerAction::Normal));
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 1);
        assert_eq!(game.players.get_mut(Player::Player2).score_red, 0);
    }

    #[test]
    fn place_next_to_red() {
        let mut game = TnEGame::new();
        game.process(Action::MoveLeader { movement: Movement::Place(pos!(0, 1)), leader: Leader::Green }).unwrap();
        let ret = game.board.find_empty_leader_space_next_to_red();
        assert_eq!(ret.iter().filter(|i|i.x < 5 && i.y < 5).count(), 3);
        game.process(Action::PlaceTile { to: pos!(1, 0), tile_type: TileType::Green }).unwrap();
        assert_eq!(game.players.get_mut(Player::Player1).score_green, 1);
    }

    #[test]
    fn find_kingdom_adj() {
        let mut game = TnEGame::new();
        game.process(Action::MoveLeader { movement: Movement::Place(pos!(1, 2)), leader: Leader::Green }).unwrap();
        let ret = game.board.find_empty_spaces_adj_kingdom(pos!(1, 1), false);

        assert_eq!(ret, vec![
            pos!(0,1),
            pos!(2,1),
            pos!(1,0),
            pos!(0,2),
            pos!(2,2),
            pos!(1,3),
        ]);
    }

    #[test]
    fn external_conflict_resolution() {
        // TODO:
    }
}
