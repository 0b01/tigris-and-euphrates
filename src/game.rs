use primitive_types::U256;
use serde::{Deserialize, Serialize};
use once_cell::sync::Lazy;
pub const W: usize = 16;
pub const H: usize = 11;
#[macro_export]
macro_rules! pos {
    // compile time convert 1A to 00
    ($s:expr) => {
        pos($s)
    };

    ($x:expr, $y:expr) => {
        Pos { x: $x as u8, y: $y as u8 }
    };
}

static NEIGHBORS_MASK: Lazy<[[Bitboard; W]; H]> = Lazy::new(|| {
    let mut mask = [[Bitboard::new(); W]; H];
    for r in 0..H {
        for c in 0..W {
            let pos = pos!(r as u8, c as u8);
            if pos.x > 0 {
                mask[r][c].set(pos!(pos.x - 1, pos.y));
            }
            if pos.x < H as u8 - 1 {
                mask[r][c].set(pos!(pos.x + 1, pos.y));
            }
            if pos.y > 0 {
                mask[r][c].set(pos!(pos.x, pos.y - 1));
            }
            if pos.y < W as u8 - 1 {
                mask[r][c].set(pos!(pos.x, pos.y + 1));
            }
        }
    }
    mask
});

static POS_TO_BITBOARD: Lazy<[[Bitboard; W]; H]> = Lazy::new(|| {
    let mut ret = [[Bitboard::new(); W]; H];
    for x in 0..H {
        for y in 0..W {
            let pos = pos!(x as u8, y as u8);
            let mut bitboard = Bitboard::new();
            bitboard.0 |= U256::one() << pos.index();
            ret[x][y] = bitboard;
        }
    }
    ret
});

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Players(pub [PlayerState; 2]);
impl Players {
    pub fn get(&self, player: Player) -> &PlayerState {
        match player {
            Player::Player1 => &self.0[0],
            Player::Player2 => &self.0[1],
            Player::None => unreachable!(),
        }
    }
    pub fn get_mut(&mut self, player: Player) -> &mut PlayerState {
        match player {
            Player::Player1 => &mut self.0[0],
            Player::Player2 => &mut self.0[1],
            Player::None => unreachable!(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Monument {
    pub monument_type: MonumentType,
    pub pos_top_left: Pos,
}

/// record last N moves
#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct MoveHistory<const N: usize> {
    pub last_placed_tiles: Vec<Option<Pos>>,
    pub last_moved_leaders: Vec<Option<Pos>>,
}

impl<const N: usize> MoveHistory<N> {
    pub const N: usize = N;

    pub fn new() -> Self {
        Self {
            last_placed_tiles: vec![None; N],
            last_moved_leaders: vec![None; N],
        }
    }

    pub fn record(&mut self, action: &Action) {
        // always keep length = N

        self.last_placed_tiles.remove(0);
        self.last_moved_leaders.remove(0);

        let (t, l) = match action {
            Action::PlaceTile { pos: to, .. } => (Some(*to), None),
            Action::PlaceLeader { pos, .. } => (None, Some(*pos)),
            _ => (None, None),
        };

        self.last_moved_leaders.push(l);
        self.last_placed_tiles.push(t);
    }
}

#[cfg_attr(feature="python", pyo3::pyclass)]
#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct TnEGame {
    pub board: Board,
    pub players: Players,
    pub move_history: MoveHistory<6>,
    bag: Tiles,

    pub available_monuments: Vec<MonumentType>,
    pub monuments: Vec<Monument>,

    pub state: Vec<GameState>,

    pub play_action_stack: Vec<(Player, PlayerAction)>,
    player_turn: Player,

    /// the last player who executed 2 actions
    pub last_player: Player,

    /// the last player who executed a normal action
    pub last_player_normal_action: Player,

    /// the last player who executed an action
    pub last_action_player: Player,

    /// Revolt
    pub internal_conflict: Option<Conflict>,

    /// War
    pub external_conflict: Option<ExternalConflict>,
}

impl std::fmt::Debug for TnEGame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TnE")
            .field("board", &self.board)
            .field("players", &self.players)
            .field("bag", &self.bag)
            .field("state", &self.state)
            .field("play_action_stack", &self.play_action_stack)
            .field("player_turn", &self.player_turn)
            .field("last_player", &self.last_player)
            .field("internal_conflict", &self.internal_conflict)
            .field("external_conflict", &self.external_conflict)
            .finish()
    }
}

#[derive(Default)]
pub struct Kingdom {
    pub map: Bitboard,
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
            move_history: MoveHistory::new(),
            available_monuments: vec![
                MonumentType::RedGreen,
                MonumentType::RedBlue,
                MonumentType::GreenBlue,
                MonumentType::BlackRed,
                MonumentType::BlackGreen,
                MonumentType::BlackBlue,
            ],
            monuments: vec![],
            bag: Tiles {
                red: 57,
                black: 30,
                green: 30,
                blue: 36,
            },
            play_action_stack: vec![
                (Player::Player1, PlayerAction::Normal),
                (Player::Player1, PlayerAction::Normal),
            ],
            player_turn: Player::Player1,
            internal_conflict: None,
            external_conflict: None,
            last_player: Player::None,
            last_action_player: Player::None,
            last_player_normal_action: Player::None,
        };

        // initial draw for players, each player draws 6 tiles
        for player in &mut game.players.0 {
            game.bag.player_draw(player, 6);
        }

        game
    }
}

impl TnEGame {
    pub fn validate_action(&self, action: Action, state: GameState) -> Result<()> {
        let current_player = self.next_player();
        let curr_player_state = self.players.get(current_player);
        let kingdom_count = self.board.nearby_kingdom_count();

        let is_valid = match (state, action) {
            (GameState::Normal, Action::PlaceTile { .. })
            | (GameState::Normal, Action::PlaceLeader { .. })
            | (GameState::Normal, Action::WithdrawLeader(_))
            | (GameState::Normal, Action::ReplaceTile(_))
            | (GameState::Normal, Action::PlaceCatastrophe(_))
            | (GameState::Normal, Action::Pass)
            | (GameState::TakeTreasure, Action::TakeTreasure { .. })
            | (GameState::WarSelectLeader, Action::WarSelectLeader(_))
            | (GameState::BuildMonument, Action::BuildMonument(_))
            | (GameState::AddSupport, Action::AddSupport(_)) => true,
            _ => false,
        };

        if !is_valid {
            return Err(Error::InvalidAction(state, action));
        }

        match action {
            Action::TakeTreasure(pos) => {
                if !self.board.get_treasure(pos) {
                    return Err(Error::NoTreasure);
                }
            }
            Action::AddSupport(n) => {
                let PlayerAction::AddSupport(tile_type) = self.next_action() else {
                    unreachable!()
                };
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
                            .find(|c| c.conflict_leader.as_tile_type() == tile_type);
                        if c.is_none() {
                            return Err(Error::NoExternalConflict);
                        }
                        c.unwrap()
                    }
                    _ => return Err(Error::NotInConflict),
                };

                // the conflict must still be in progress
                // leaders must still be connected
                if !self
                    .board
                    .path_find(conflict.attacker_pos, conflict.defender_pos)
                {
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
            Action::WarSelectLeader(leader) => {
                // we must be in an external conflict
                let Some(conflicts) = &self.external_conflict else {
                    return Err(Error::NoExternalConflict);
                };

                // the leader must still be in conflict
                let Some(conflict) = conflicts.conflicts.iter().find(|c| c.conflict_leader == leader) else {
                    return Err(Error::LeaderNotInConflict);
                };

                // leaders must still be connected
                if !self
                    .board
                    .path_find(conflict.attacker_pos, conflict.defender_pos)
                {
                    return Err(Error::LeadersDisconnected);
                }
            }
            Action::PlaceCatastrophe(to) => {
                if curr_player_state.num_catastrophes == 0 {
                    return Err(Error::NoCatastrophes);
                }
                // cannot be placed if the destination contains a leader
                if self.board.get_monument(to) {
                    return Err(Error::CannotPlaceCatastropheOnMonument);
                }
                if self.board.get_leader(to) != Leader::None {
                    return Err(Error::CannotPlaceOverLeader);
                }
                // cannot place over unclaimed treasure
                if self.board.get_treasure(to) {
                    return Err(Error::CannotPlaceOverTreasure);
                }
            }
            Action::PlaceTile { pos: to, tile_type } => {
                if self.board.get_tile_type(to) != TileType::Empty {
                    return Err(Error::CannotPlaceTileOverTile);
                }
                if self.board.get_leader(to) != Leader::None {
                    return Err(Error::CannotPlaceOverLeader);
                }
                if curr_player_state.get_hand(tile_type) == 0 {
                    return Err(Error::NotEnoughTiles);
                }

                // only blue can be and must be placed on river
                let is_river = RIVER.get(to);
                if let TileType::Blue = tile_type {
                    if !is_river {
                        return Err(Error::MustPlaceBlueOnRiver);
                    }
                } else if is_river {
                    return Err(Error::CannotPlaceNonBlueOnRiver);
                }

                // cannot join three kingdoms
                if kingdom_count[to.x as usize][to.y as usize] > 2 {
                    return Err(Error::CannotJoinThreeKingdoms);
                }
            }
            Action::PlaceLeader {
                pos,
                ..
            } => {
                if self.board.get_leader(pos) != Leader::None {
                    return Err(Error::CannotPlaceOverLeader);
                }
                self.check_leader_placement(pos, kingdom_count)?;
            }
            Action::WithdrawLeader(pos) => {
                if self.board.get_leader(pos) == Leader::None {
                    return Err(Error::CannotWithdrawLeader);
                }
            }
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
            Action::BuildMonument(monument_type) => {
                let PlayerAction::BuildMonument(pos_top_left, ..) = self.next_action() else {
                    unreachable!()
                };
                let pos_top_right = pos_top_left + pos!(0, 1);
                let pos_bottom_left = pos_top_left + pos!(1, 0);
                let pos_bottom_right = pos_top_left + pos!(1, 1);
                if self.board.get_tile_type(pos_top_left) != self.board.get_tile_type(pos_top_right)
                    || self.board.get_tile_type(pos_top_left) != self.board.get_tile_type(pos_bottom_left)
                    || self.board.get_tile_type(pos_top_left) != self.board.get_tile_type(pos_bottom_right)
                    || !monument_type.matches(self.board.get_tile_type(pos_top_left))
                {
                    return Err(Error::MonumentNot2x2);
                }
            }
        }

        Ok(())
    }

    pub fn process_action(
        &mut self,
        action: Action,
        current_player: Player,
        current_player_action: PlayerAction,
    ) -> (Option<GameState>, Vec<(Player, PlayerAction, GameState)>) {
        let curr_player = self.players.get_mut(current_player);
        let mut extra_actions = Vec::new();

        let next_state = match action {
            Action::BuildMonument(monument_type) => {
                let PlayerAction::BuildMonument(pos_top_left, ..) = current_player_action else {
                    panic!();
                };
                let positions = [
                    pos_top_left,
                    pos_top_left.right(),
                    pos_top_left.down(),
                    pos_top_left.down().right(),
                ];
                for pos in positions.iter() {
                    self.board.set_monument(*pos, true);
                    self.board.set_leader(*pos, Leader::None);
                    self.board.set_tile_type(*pos, TileType::Empty);
                }

                self.monuments.push(Monument {
                    monument_type,
                    pos_top_left,
                });

                None
            }
            Action::TakeTreasure(pos) => {
                self.board.set_treasure(pos, false);

                curr_player.score_treasure += 1;
                None
            }
            Action::WarSelectLeader(leader) => {
                assert!(self.internal_conflict.is_none());
                let Some(conflicts) = &mut self.external_conflict else {
                    unreachable!();
                };
                let Some(conflict) = conflicts.conflicts.iter_mut().find(|c| c.conflict_leader == leader) else {
                    unreachable!();
                };

                let next_action = PlayerAction::AddSupport(conflict.conflict_leader.as_tile_type());
                self.play_action_stack
                    .push((conflict.attacker, next_action));
                Some(GameState::AddSupport)
            }
            Action::AddSupport(n) => {
                let PlayerAction::AddSupport(tile_type) = current_player_action else {
                    unreachable!()
                };
                let c = match (
                    self.internal_conflict.as_mut(),
                    self.external_conflict.as_mut(),
                ) {
                    (Some(c), _) => c,
                    (_, Some(conflicts)) => {
                        let Some(c) = conflicts.conflicts.iter_mut().find(|c| c.conflict_leader.as_tile_type() == tile_type) else {
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

                        let (winner, _loser, loser_pos) = if attacker_points > defender_points {
                            (attacker, defender, c.defender_pos)
                        } else {
                            (defender, attacker, c.attacker_pos)
                        };

                        // give winner 1 point
                        self.players
                            .get_mut(winner)
                            .add_score(c.conflict_leader.as_tile_type());

                        self.evict_leader(loser_pos);
                    } else {
                        // external conflict
                        let attacker_points = c.attacker_base_strength + c.attacker_support;
                        let defender_points = c.defender_base_strength + c.defender_support;

                        let (winner, loser, loser_pos) = if attacker_points > defender_points {
                            (attacker, defender, c.defender_pos)
                        } else {
                            (defender, attacker, c.attacker_pos)
                        };

                        let tiles_to_remove = self
                            .board
                            .find_disintegrable_tiles(loser_pos, c.conflict_leader.as_tile_type());

                        // remove loser
                        self.players.get_mut(loser).set_leader(c.conflict_leader, None);
                        self.board.set_leader(loser_pos, Leader::None);
                        self.board.set_player(loser_pos, Player::None);
                        // give winner 1 point
                        self.players
                            .get_mut(winner)
                            .add_score(c.conflict_leader.as_tile_type());

                        // disintegrate tiles
                        let points = tiles_to_remove.count_ones() as u8;
                        for pos in tiles_to_remove.iter() {
                            self.board.set_tile_type(pos, TileType::Empty);
                            self.board.set_leader(pos, Leader::None);
                            self.board.set_player(pos, Player::None)
                        }
                        self.players
                            .get_mut(winner)
                            .add_score_by(c.conflict_leader.as_tile_type(), points);

                        let leader = c.conflict_leader;

                        // remove the leader we just resolved from external conflicts
                        self.external_conflict
                            .as_mut()
                            .unwrap()
                            .conflicts
                            .retain(|con| con.conflict_leader != leader);

                        // credit unification cell
                        let ExternalConflict {
                            unification_tile_pos: uni_pos,
                            unification_tile_type,
                            ..
                        } = self.external_conflict.as_ref().unwrap();
                        // revert to original
                        self.board.set_tile_type(*uni_pos, *unification_tile_type);
                        let mut visited = Bitboard::new();
                        let kingdom = self.board.find_kingdom(*uni_pos, &mut visited);
                        if let Some((p, _, _)) =
                            kingdom.get_leader_info(self.board.get_tile_type(*uni_pos).as_leader())
                        {
                            self.players
                                .get_mut(p)
                                .add_score(self.board.get_tile_type(*uni_pos));
                        }
                    }

                    // defender draws back to 6
                    let defender = self.players.get_mut(defender);
                    match self.bag.draw_to_6(defender) {
                        Some(_) => {
                            self.internal_conflict = None;
                        }
                        None => return (None, vec![]),
                    };

                    // if external conflict, check if there's another leader we can select
                    if let Some(ex) = &mut self.external_conflict {
                        ex.conflicts
                            .retain(|i| self.board.path_find(i.attacker_pos, i.defender_pos));
                        if ex.conflicts.is_empty() {
                            let uni_pos = self.external_conflict.as_ref().unwrap().unification_tile_pos;
                            let uni_tile_type = self.external_conflict.as_ref().unwrap().unification_tile_type;
                            self.check_treasure_at(uni_pos, &mut extra_actions);
                            self.check_monument(uni_pos, uni_tile_type, &mut extra_actions);

                            self.external_conflict = None;

                            Some(GameState::Normal)
                        } else {
                            self.play_action_stack.push((
                                attacker,
                                PlayerAction::SelectLeader {
                                    red: ex.conflicts.iter().any(|i| i.conflict_leader == Leader::Red),
                                    black: ex.conflicts.iter().any(|i| i.conflict_leader == Leader::Black),
                                    green: ex.conflicts.iter().any(|i| i.conflict_leader == Leader::Green),
                                    blue: ex.conflicts.iter().any(|i| i.conflict_leader == Leader::Blue),
                                },
                            ));
                            Some(GameState::WarSelectLeader)
                        }
                    } else {
                        // *check_treasure = true;
                        Some(GameState::Normal)
                    }
                } else {
                    let tile_type = if c.is_internal {
                        TileType::Red
                    } else {
                        c.conflict_leader.as_tile_type()
                    };
                    self.play_action_stack
                        .push((c.defender, PlayerAction::AddSupport(tile_type)));
                    Some(GameState::AddSupport)
                }
            }
            Action::PlaceCatastrophe(to) => {
                self.board.set_catastrophe(to, true);
                self.board.set_player(to, Player::None);
                self.board.set_leader(to, Leader::None);
                self.board.set_tile_type(to, TileType::Empty);

                curr_player.num_catastrophes -= 1;

                for neighbor in to.neighbors().iter() {
                    self.evict_leader(neighbor);
                }

                Some(GameState::Normal)
            }
            Action::PlaceTile { pos: to, tile_type } => {
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
                            conflict_leader: leader,
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
                    let red = external_conflicts.iter().any(|c| c.conflict_leader == Leader::Red);
                    let black = external_conflicts.iter().any(|c| c.conflict_leader == Leader::Black);
                    let green = external_conflicts.iter().any(|c| c.conflict_leader == Leader::Green);
                    let blue = external_conflicts.iter().any(|c| c.conflict_leader == Leader::Blue);

                    self.board.unification_tile = Some(to);
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
                    self.board.set_tile_type(to, tile_type);
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
                        .find_map(|k| k.get_leader_info(tile_type.as_leader()).map(|(p, _, _)| p));

                    // if the matching leader is not in the kingdom, check if black leader is in the kingdom
                    // and if so, score for black leader
                    let black_leader = kingdoms
                        .iter()
                        .find_map(|k| k.get_leader_info(Leader::Black).map(|(p, _, _)| p));

                    match (matching_leader, black_leader) {
                        (Some(p), _) | (None, Some(p)) => {
                            self.players.get_mut(p).add_score(tile_type)
                        }
                        (None, None) => (),
                    }
                    self.check_treasure_at(to, &mut extra_actions);
                    self.check_monument(to, tile_type, &mut extra_actions);
                    Some(GameState::Normal)
                }
            }
            Action::PlaceLeader { pos, leader } => {
                if let Some(from) = curr_player.get_leader(leader) {
                    self.board.set_leader(from, Leader::None);
                    self.board.set_player(from, Player::None);
                }

                let next_state = self.check_internal_conflict(pos, leader, current_player);

                if next_state == GameState::Normal {
                    self.check_treasure_at(pos, &mut extra_actions);
                }

                self.board.set_leader(pos, leader);
                self.board.set_player(pos, current_player);
                self.players
                    .get_mut(current_player)
                    .set_leader(leader, Some(pos));
                Some(next_state)
            }
            Action::WithdrawLeader(pos) => {
                self.evict_leader(pos);
                Some(GameState::Normal)
            },
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
            Action::Pass => Some(GameState::Normal),
        };

        // check if game is over
        if self.board.available_treasures_count() == 2 || self.bag.is_empty() {
            (None, extra_actions)
        } else {
            (next_state, extra_actions)
        }
    }

    pub(crate) fn check_internal_conflict(
        &mut self,
        pos: Pos,
        leader: Leader,
        current_player: Player,
    ) -> GameState {
        // check internal conflict
        // if the kingdom already contains a leader, then an internal conflict is triggered
        let kingdoms = self.board.neighboring_kingdoms(pos);

        let mut has_conflict = None;
        for kingdom in kingdoms {
            has_conflict = match leader {
                Leader::Red
                    if kingdom.red_leader.is_some()
                        && kingdom.red_leader.unwrap().0 != current_player =>
                {
                    kingdom.red_leader
                }
                Leader::Black
                    if kingdom.black_leader.is_some()
                        && kingdom.black_leader.unwrap().0 != current_player =>
                {
                    kingdom.black_leader
                }
                Leader::Green
                    if kingdom.green_leader.is_some()
                        && kingdom.green_leader.unwrap().0 != current_player =>
                {
                    kingdom.green_leader
                }
                Leader::Blue
                    if kingdom.blue_leader.is_some()
                        && kingdom.blue_leader.unwrap().0 != current_player =>
                {
                    kingdom.blue_leader
                }
                _ => None,
            };
        }

        if let Some((player_2, p2_pos)) = has_conflict {
            let attacker_base_strength = pos
                .neighbors().iter()
                .map(|n| self.board.get_tile_type(n))
                .filter(|t| *t == TileType::Red)
                .count() as u8;
            let defender_base_strength = p2_pos
                .neighbors().iter()
                .map(|n| self.board.get_tile_type(n))
                .filter(|t| *t == TileType::Red)
                .count() as u8;
            self.internal_conflict = Some(Conflict {
                is_internal: true,
                attacker: current_player,
                defender: player_2,
                conflict_leader: leader,
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

    fn set_next_player_if_player_turn_over(&mut self) {
        // if is turn over
        if self.play_action_stack.is_empty() {
            let current_player = self.player_turn;

            // add points from monuments
            for Monument {
                monument_type,
                pos_top_left: monument_top_left,
            } in &self.monuments
            {
                let mut visited = Bitboard::new();
                let kingdom = self.board.find_kingdom(*monument_top_left, &mut visited);
                for leader in monument_type.unpack() {
                    if let Some((p, _, _)) = kingdom.get_leader_info(leader) {
                        if p == current_player {
                            self.players
                                .get_mut(current_player)
                                .add_score(leader.as_tile_type());
                        }
                    }
                }
            }

            let opposite = match current_player {
                Player::None => unreachable!(),
                Player::Player1 => Player::Player2,
                Player::Player2 => Player::Player1,
            };

            self.play_action_stack
                .push((opposite, PlayerAction::Normal));
            self.play_action_stack
                .push((opposite, PlayerAction::Normal));
            self.player_turn = opposite;

            if self.bag.draw_to_6(&mut self.players.0[0]).is_none() {
                self.state.clear();
            }
            if self.bag.draw_to_6(&mut self.players.0[1]).is_none() {
                self.state.clear();
            }
        }
    }

    pub fn process(&mut self, action: Action) -> Result<(Player, PlayerAction)> {
        let current_state = self.state.pop().ok_or(Error::GameOver)?;
        if let Err(e) = self.validate_action(action, current_state) {
            self.state.push(current_state);
            return Err(e);
        }
        if current_state == GameState::Normal {
            self.move_history.record(&action);
        }
        let (current_player, current_player_action) = self.play_action_stack.pop().unwrap();
        if current_state == GameState::Normal {
            self.last_player_normal_action = current_player;
        }
        let (next_state, extra_actions) = self.process_action(action, current_player, current_player_action);
        if let Some(next_state) = next_state {
            // println!("{:?} => {:?}, {action:?}", current_state, next_state);
            self.state.push(next_state);
        }

        for (player, action, state) in extra_actions {
            self.play_action_stack.push((player, action));
            self.state.push(state);
        }

        self.last_action_player = current_player;
        self.set_next_player_if_player_turn_over();

        self.last_player = current_player;
        Ok(self.next())
    }

    fn check_treasure_at(&self, pos: Pos, extra_actions: &mut Vec<(Player, PlayerAction, GameState)>) {
        // check if treasure is available for taking
        // TODO: must take corner treasures first
        let kingdom = self.board.find_kingdom(pos, &mut Bitboard::new());
        if kingdom.treasures.iter().filter(|i| i.is_some()).count() > 1
            && kingdom.green_leader.is_some()
        {
            let green_player = kingdom.green_leader.unwrap().0;
            let treasures = kingdom.treasures.map(Option::unwrap);
            extra_actions.push((green_player, PlayerAction::TakeTreasure(treasures), GameState::TakeTreasure))
        }
    }

    fn check_monument(&self, pos: Pos, color: TileType, extra_actions: &mut Vec<(Player, PlayerAction, GameState)>) {
        // monument can be built on a 2x2 square made of same tiles
        // check if monument is available for building
        let mut monument = false;
        let mut top_left = None;
        // if pos is top left
        if pos.x < H as u8 - 1 && pos.y < W as u8 - 1 {
            let to_check = [pos, pos.right(), pos.down(), pos.down().right()];
            if to_check
                .iter()
                .all(|p| self.board.get_tile_type(*p) == color)
            {
                monument = true;
                top_left = Some(pos);
            }
        }
        // if pos is top right
        if pos.x < H as u8 - 1 && pos.y > 0 {
            let to_check = [pos, pos.left(), pos.down(), pos.down().left()];
            if to_check
                .iter()
                .all(|p| self.board.get_tile_type(*p) == color)
            {
                monument = true;
                top_left = Some(pos.left());
            }
        }
        // if pos is bottom left
        if pos.x > 0 && pos.y < W as u8 - 1 {
            let to_check = [pos, pos.right(), pos.up(), pos.up().right()];
            if to_check
                .iter()
                .all(|p| self.board.get_tile_type(*p) == color)
            {
                monument = true;
                top_left = Some(pos.up());
            }
        }
        // if pos is bottom right
        if pos.x > 0 && pos.y > 0 {
            let to_check = [pos, pos.left(), pos.up(), pos.up().left()];
            if to_check
                .iter()
                .all(|p| self.board.get_tile_type(*p) == color)
            {
                monument = true;
                top_left = Some(pos.up().left());
            }
        }

        let avail = self
            .available_monuments
            .iter()
            .filter(|c| c.matches(color))
            .copied()
            .collect::<Vec<_>>();

        if monument && !avail.is_empty() {
            extra_actions.push((
                self.last_player_normal_action,
                PlayerAction::BuildMonument(top_left.unwrap(), avail),
                GameState::BuildMonument
            ))
        }
    }

    #[must_use]
    fn adjacent_to_temple(&self, pos: Pos) -> bool {
        for neighbor in pos.neighbors().iter() {
            if self.board.get_tile_type(neighbor) == TileType::Red {
                return true;
            }
        }
        false
    }

    fn check_leader_placement(&self, pos: Pos, kingdom_count: [[u8; W]; H]) -> Result<()> {
        if self.board.get_leader(pos) != Leader::None {
            return Err(Error::CannotPlaceOverLeader);
        }

        if self.board.get_tile_type(pos) != TileType::Empty {
            return Err(Error::CannotPlaceTileOverTile);
        }

        // must be adjacent to a temple
        if !self.adjacent_to_temple(pos) {
            return Err(Error::CannotPlaceLeaderNotAdjacentToTemple);
        }

        // cannot be placed on river or catastrophe
        if RIVER.get(pos) {
            return Err(Error::CannotPlaceLeaderOnRiver);
        }

        if self.board.get_catastrophe(pos) {
            return Err(Error::CannotPlaceLeaderOnCatastrophe);
        }

        // a leader cannot join two kingdoms
        if kingdom_count[pos.x as usize][pos.y as usize] > 1 {
            return Err(Error::CannotPlaceLeaderJoiningTwoKingdoms);
        }

        Ok(())
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
        self.play_action_stack
            .last()
            .map(|(_, a)| a.clone())
            .unwrap()
    }

    pub fn next_player(&self) -> Player {
        self.play_action_stack.last().map(|(p, _)| *p).unwrap()
    }

    pub fn next(&self) -> (Player, PlayerAction) {
        self.play_action_stack.last().cloned().unwrap()
    }

    pub fn next_state(&self) -> GameState {
        self.state.last().copied().unwrap()
    }

    fn evict_leader(&mut self, pos: Pos) {
        let player = self.board.get_player(pos);
        if self.board.get_leader(pos) != Leader::None {
            self.players
                .get_mut(player)
                .set_leader(self.board.get_leader(pos), None);
            self.board.set_leader(pos, Leader::None);
            self.board.set_player(pos, Player::None)
        }
    }
}

#[derive(Copy, PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub enum GameState {
    Normal,
    TakeTreasure,
    AddSupport,
    WarSelectLeader,
    BuildMonument,
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
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
    BuildMonument(Pos, Vec<MonumentType>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExternalConflict {
    pub conflicts: Vec<Conflict>,
    pub unification_tile_pos: Pos,
    pub unification_tile_type: TileType,
}

/// occurs when placing same leaders in same kingdom
#[derive(Debug, Clone, Copy, Eq, Serialize, Deserialize)]
pub struct Conflict {
    is_internal: bool,
    pub conflict_leader: Leader,

    attacker: Player,
    pub attacker_pos: Pos,
    attacker_support: u8,
    attacker_sent_support: bool,
    attacker_base_strength: u8,

    defender: Player,
    pub defender_pos: Pos,
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
            && self.conflict_leader == other.conflict_leader
            && self.attacker_pos == other.attacker_pos
            && self.defender_pos == other.defender_pos
            && self.attacker_base_strength == other.attacker_base_strength
            && self.defender_base_strength == other.defender_base_strength
            && self.is_internal == other.is_internal
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    WarSelectLeader(Leader),
    AddSupport(u8),
    BuildMonument(MonumentType),
    PlaceTile {
        pos: Pos,
        tile_type: TileType,
    },
    TakeTreasure(Pos),
    PlaceLeader {
        pos: Pos,
        leader: Leader,
    },
    WithdrawLeader(Pos),
    ReplaceTile(Tiles),
    PlaceCatastrophe(Pos),
    Pass,
}

/// PlaceTileRed, 11 * 16
/// PlaceTileGreen, 11 * 16
/// PlaceTileBlue, 11 * 16
/// PlaceTileBlack, 11 * 16
/// PlaceLeaderRed, 11 * 16
/// PlaceLeaderGreen, 11 * 16
/// PlaceLeaderBlue, 11 * 16
/// PlaceLeaderBlack, 11 * 16
/// WithdrawLeader, 11 * 16
/// PlaceCatastrophe, 11 * 16
/// TakeTreasure, 11 * 16
/// BuildMonument, 6
/// AddSupport, 7
/// SelectLeader, 4
/// Pass, 1
impl Into<usize> for Action {
    fn into(self) -> usize {
        match self {
            Action::PlaceTile { pos: to, tile_type } => {
                let i = to.x as usize * W + to.y as usize;
                match tile_type {
                    TileType::Red => 0 * H * W + i,
                    TileType::Green => 1 * H * W + i,
                    TileType::Blue => 2 * H * W + i,
                    TileType::Black => 3 * H * W + i,
                    _ => unreachable!(),
                }
            }
            Action::PlaceLeader { pos, leader } => {
                let i = pos.x as usize * W + pos.y as usize;
                match leader {
                    Leader::Red => 4 * H * W + i,
                    Leader::Green => 5 * H * W + i,
                    Leader::Blue => 6 * H * W + i,
                    Leader::Black => 7 * H * W + i,
                    _ => unreachable!(),
                }
            }
            Action::WithdrawLeader(pos) => {
                let i = pos.x as usize * W + pos.y as usize;
                8 * H * W + i
            }
            Action::PlaceCatastrophe(pos) => {
                let i = pos.x as usize * W + pos.y as usize;
                9 * H * W + i
            }
            Action::TakeTreasure(pos) => {
                let i = pos.x as usize * W + pos.y as usize;
                10 * H * W + i
            }
            Action::BuildMonument(ty) => {
                11 * H * W + (ty as usize)
            }
            Action::AddSupport(n) => {
                11 * H * W + 6 + (n as usize)
            }
            Action::WarSelectLeader(leader) => {
                11 * H * W + 6 + 7 + (leader as usize - 1)
            }
            Action::Pass => {
                11 * H * W + 6 + 7 + 4
            }
            Action::ReplaceTile(_) => unreachable!(),
        }
    }
}

impl From<usize> for Action {
    fn from(n: usize) -> Self {
        let idx = n / (W * H);
        let offset = n % (W * H);
        let pos = pos!(offset / W, offset % W);
        match idx {
            0 => Action::PlaceTile { pos, tile_type: TileType::Red },
            1 => Action::PlaceTile { pos, tile_type: TileType::Green },
            2 => Action::PlaceTile { pos, tile_type: TileType::Blue },
            3 => Action::PlaceTile { pos, tile_type: TileType::Black },
            4 => Action::PlaceLeader { pos, leader: Leader::Red },
            5 => Action::PlaceLeader { pos, leader: Leader::Green },
            6 => Action::PlaceLeader { pos, leader: Leader::Blue },
            7 => Action::PlaceLeader { pos, leader: Leader::Black },
            8 => Action::WithdrawLeader(pos),
            9 => Action::PlaceCatastrophe(pos),
            10 => Action::TakeTreasure(pos),
            _ => {
                let idx = n - (W * H * 11);
                match idx {
                    0 => Action::BuildMonument(MonumentType::RedGreen),
                    1 => Action::BuildMonument(MonumentType::RedBlue),
                    2 => Action::BuildMonument(MonumentType::GreenBlue),
                    3 => Action::BuildMonument(MonumentType::BlackRed),
                    4 => Action::BuildMonument(MonumentType::BlackGreen),
                    5 => Action::BuildMonument(MonumentType::BlackBlue),

                    6 => Action::AddSupport(idx as u8 - 6),
                    7 => Action::AddSupport(idx as u8 - 6),
                    8 => Action::AddSupport(idx as u8 - 6),
                    9 => Action::AddSupport(idx as u8 - 6),
                    10 => Action::AddSupport(idx as u8 - 6),
                    11 => Action::AddSupport(idx as u8 - 6),
                    12 => Action::AddSupport(idx as u8 - 6),

                    13 => Action::WarSelectLeader(Leader::Blue),
                    14 => Action::WarSelectLeader(Leader::Green),
                    15 => Action::WarSelectLeader(Leader::Red),
                    16 => Action::WarSelectLeader(Leader::Black),

                    17 => Action::Pass,

                    _ => panic!(),
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Tiles {
    pub red: u8,
    pub black: u8,
    pub green: u8,
    pub blue: u8,
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

    pub fn count(&self, tile_type: TileType) -> u8 {
        match tile_type {
            TileType::Red => self.red,
            TileType::Black => self.black,
            TileType::Green => self.green,
            TileType::Blue => self.blue,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Movement {
    Place(Pos),
    Withdraw(Pos),
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pos {
    pub x: u8,
    pub y: u8,
}

impl std::ops::Add for Pos {
    type Output = Pos;

    fn add(self, other: Pos) -> Pos {
        Pos {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::fmt::Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let col = (self.y as u8 + 'A' as u8) as char;
        let row = self.x + 1;
        write!(f, "{}{}", row, col)
    }
}

pub const fn pos(a: &'static str) -> Pos {
    let chars = a.as_bytes();
    let x = chars[0] as u8 - '1' as u8;
    let y = chars[1] as u8 - 'A' as u8;
    Pos { x, y }
}

impl Pos {
    fn neighbors(&self) -> Bitboard {
        NEIGHBORS_MASK[self.x as usize][self.y as usize]
    }

    fn right(&self) -> Pos {
        Self {
            x: self.x,
            y: self.y + 1,
        }
    }

    fn left(&self) -> Pos {
        Self {
            x: self.x,
            y: self.y - 1,
        }
    }

    fn up(&self) -> Pos {
        Self {
            x: self.x - 1,
            y: self.y,
        }
    }

    fn down(&self) -> Pos {
        Self {
            x: self.x + 1,
            y: self.y,
        }
    }

    fn index(&self) -> usize {
        self.x as usize * W + self.y as usize
    }

    fn from_index(index: usize) -> Pos {
        Pos {
            x: (index / W) as u8,
            y: (index % W) as u8,
        }
    }

    fn mask(&self) -> Bitboard {
        POS_TO_BITBOARD[self.x as usize][self.y as usize]
    }
}

#[repr(packed)]
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
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

        // 100 points for each final score
        s += self.calculate_score() as i16 * 20;

        for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].into_iter() {
            if let Some(pos) = self.get_leader(leader) {
                // 5 points for nearby red tiles
                for p in pos.neighbors().iter() {
                    if state.board.get_tile_type(p) == TileType::Red {
                        s += 10;
                    }
                }

                // 5 points for each matching tile in kingdom
                let mut visited = Bitboard::new();
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

/// Bitboard for fast operations on the board 
#[derive(Default, Copy, Clone, Eq, PartialEq, Serialize, Deserialize,
    derive_more::Not,
    derive_more::BitXor, derive_more::BitAnd, derive_more::BitOr,
    derive_more::BitOrAssign, derive_more::BitAndAssign, derive_more::BitXorAssign
)]
pub struct Bitboard(pub U256);

impl std::fmt::Display for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        s.push('\n');
        for x in 0..H {
            for y in 0..W {
                if self.get(pos!(x as u8, y as u8)) {
                    s.push('⬛');
                } else {
                    s.push('🟨');
                }
            }
            s.push('\n');
        }
        write!(f, "{}", s)
    }
}

impl std::fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Bitboard {
    pub fn new() -> Self {
        Self(U256::from(0))
    }

    pub const fn from_binary(s: [u64; 4]) -> Self {
        Self(U256(s))
    }

    pub fn iter(&self) -> impl Iterator<Item = Pos> + '_ {
        let mut mask = self.0;
        std::iter::from_fn(move || {
            if mask.is_zero() {
                None
            } else {
                let pos = mask.trailing_zeros() as usize;
                mask &= !(U256::one() << U256::from(pos));
                Some(pos!(pos as u8 / W as u8, pos as u8 % W as u8))
            }
        })
    }

    pub fn count_ones(&self) -> u8 {
        self.0.0.iter().map(|x| x.count_ones() as u8).sum()
    }

    pub fn set(&mut self, pos: Pos) {
        *self |= pos.mask()
    }

    pub fn clear(&mut self) {
        self.0 = U256::zero();
    }

    pub fn reset(&mut self, pos: Pos) {
        *self &= !pos.mask();
    }

    pub fn get(&self, pos: Pos) -> bool {
        (*self & pos.mask()).0 != U256::zero()
    }

    fn pop(&mut self) -> Option<Pos> {
        // pop the first 1 in the U256
        let mut i = 0;
        while i < 4 {
            if self.0.0[i] != 0 {
                let j = self.0.0[i].trailing_zeros() as usize;
                self.0.0[i] &= !(1 << j);
                return Some(Pos::from_index(i * 64 + j));
            }
            i += 1;
        }
        None
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Board {
    pub unification_tile: Option<Pos>,
    pub monuments: Bitboard,
    pub treasures: Bitboard,
    pub catastrophes: Bitboard,
    pub player1: Bitboard,
    pub player2: Bitboard,

    pub leader_red: Bitboard,
    pub leader_blue: Bitboard,
    pub leader_green: Bitboard,
    pub leader_black: Bitboard,

    pub red_tiles: Bitboard,
    pub blue_tiles: Bitboard,
    pub green_tiles: Bitboard,
    pub black_tiles: Bitboard,
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

pub static RIVER: Lazy<Bitboard> = Lazy::new(|| {
    let mut ret = Bitboard::new();
    for (x, row) in BOARD.iter().enumerate() {
        for (y, c) in row.chars().enumerate() {
            if c == 'x' {
                ret.set(pos!(x as u8, y as u8));
            }
        }
    }
    ret
});

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
    "......t.........",
    "..........t.....",
];

impl Board {
    /// Create a new board
    pub fn new() -> Self {
        let mut ret = Board{
            unification_tile: None,
            monuments: Bitboard::new(),
            treasures: Bitboard::new(),
            catastrophes: Bitboard::new(),
            player1: Bitboard::new(),
            player2: Bitboard::new(),

            leader_red: Bitboard::new(),
            leader_blue: Bitboard::new(),
            leader_green: Bitboard::new(),
            leader_black: Bitboard::new(),

            red_tiles: Bitboard::new(),
            blue_tiles: Bitboard::new(),
            green_tiles: Bitboard::new(),
            black_tiles: Bitboard::new(),
        };

        for (x, row) in BOARD.iter().enumerate() {
            for (y, c) in row.chars().enumerate() {
                match c {
                    't' => {
                        ret.treasures.set(pos!(x as u8, y as u8));
                        ret.set_tile_type(pos!(x as u8, y as u8), TileType::Red);
                    }
                    '.' | 'x' => {}
                    _ => unreachable!(),
                }
            }
        }

        ret
    }

    pub fn is_connectable(&self, pos: Pos) -> bool {
        self.get_tile_type(pos) != TileType::Empty
            || self.get_leader(pos) != Leader::None
            || self.unification_tile == Some(pos)
            || self.get_monument(pos)
    }

    pub fn can_place_catastrophe(&self, pos: Pos) -> bool {
        self.get_leader(pos) == Leader::None
            && self.get_tile_type(pos) != TileType::Empty
            && !self.get_treasure(pos)
            && !self.get_monument(pos)
            && !self.get_catastrophe(pos)
    }

    pub fn can_place_tile(&self, pos: Pos) -> bool {
        !self.get_catastrophe(pos)
            && !self.get_monument(pos)
            && self.get_leader(pos) == Leader::None
            && self.get_tile_type(pos) == TileType::Empty
    }

    pub fn neighboring_kingdoms(&self, to: Pos) -> Vec<Kingdom> {
        let mut kingdoms = vec![];
        let mut visited = Bitboard::new();
        for neighbor in to.neighbors().iter() {
            if visited.get(neighbor) {
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
    pub fn path_find(&self, p1_pos: Pos, p2_pos: Pos) -> bool {
        // find a path between p1_pos and p2_pos, the cells must be is_connectable()
        let mut visited = Bitboard::new();
        let mut stack = p1_pos.mask();
        while let Some(pos) = stack.pop() {
            if visited.get(pos) {
                continue;
            }
            if pos == p2_pos {
                return true;
            }
            visited.set(pos);
            for neighbor in pos.neighbors().iter() {
                if self.is_connectable(pos) {
                    stack.set(neighbor);
                }
            }
        }

        false
    }

    pub fn find_empty_leader_space_next_to_red(&self) -> Bitboard {
        let mut ret = Bitboard::new();

        let count = self.nearby_kingdom_count();

        for x in 0..H {
            for y in 0..W {
                if self.get_tile_type(pos!(x as u8, y as u8)) == TileType::Red {
                    for pos in pos!(x as u8, y as u8).neighbors().iter() {
                        if count[pos.x as usize][pos.y as usize] > 1 {
                            continue;
                        }

                        if self.get_tile_type(pos) == TileType::Empty
                            && !RIVER.get(pos)
                            && !self.get_catastrophe(pos)
                            && self.get_leader(pos) == Leader::None
                        {
                            ret.set(pos);
                        }
                    }
                }
            }
        }

        ret
    }

    pub fn nearby_kingdom_count(&self) -> [[u8; 16]; 11] {
        // for each grid cell, count number of neighboring kingdoms
        let mut visited = Bitboard::new();
        let mut count = [[0_u8; W]; H];

        for x in 0..H {
            for y in 0..W {
                let pos = pos!(x as u8, y as u8);
                if visited.get(pos) {
                    continue;
                }
                let kingdom = self.find_kingdom(pos, &mut visited);
                if !kingdom.has_leader() {
                    continue;
                }

                // find surrounding empty cells
                let neighbor_mask = kingdom.map
                    .iter()
                    .map(|p| p.neighbors())
                    .reduce(|a, b| a | b)
                    .unwrap_or_default();
                let nearby_empty_cells = !kingdom.map & neighbor_mask;

                for (rr, row) in count.iter_mut().enumerate() {
                    for (cc, c) in row.iter_mut().enumerate() {
                        if nearby_empty_cells.get(pos!(rr as u8, cc as u8)) {
                            *c += 1;
                        }
                    }
                }
            }
        }
        count
    }

    pub fn find_catastrophe_positions(&self) -> Vec<Pos> {
        let mut ret = Vec::new();
        for x in 0..H {
            for y in 0..W {
                if self.can_place_catastrophe(pos!(x as u8, y as u8)) {
                    ret.push(pos!(x as u8, y as u8));
                }
            }
        }

        ret
    }

    pub fn find_kingdom_map(&self, start: Pos, kingdom: &mut Bitboard) {
        let mut stack = start.mask();
        // if it's connectable, mark as kingdom
        while let Some(pos) = stack.pop() {
            if kingdom.get(pos) {
                continue;
            }

            if !self.is_connectable(pos) {
                continue;
            }

            kingdom.set(pos);

            stack |= pos.neighbors();
        }
    }

    pub fn find_kingdom(&self, pos: Pos, visited: &mut Bitboard) -> Kingdom {
        let mut kingdom = Kingdom::default();

        let mut stack = pos.mask();
        let mut map = Bitboard::new();

        while let Some(pos) = stack.pop() {
            if visited.get(pos) {
                continue;
            }
            *visited |= pos.mask();

            if !self.is_connectable(pos) {
                continue;
            }

            map |= pos.mask();

            if self.get_treasure(pos) {
                match &mut kingdom.treasures {
                    [None, None] => kingdom.treasures[0] = Some(pos),
                    [Some(_), None] => kingdom.treasures[1] = Some(pos),
                    [Some(_), Some(_)] => (), // just ignore if kingdom has 3 treasures
                    _ => unreachable!(),
                }
            }

            stack |= pos.neighbors();
        }

        kingdom.map = map;

        kingdom.red_tiles = (map & self.red_tiles).count_ones();
        kingdom.blue_tiles = (map & self.blue_tiles).count_ones();
        kingdom.green_tiles = (map & self.green_tiles).count_ones();
        kingdom.black_tiles = (map & self.black_tiles).count_ones();

        kingdom.red_leader = (self.leader_red & map).iter().next().map(|p| (self.get_player(p), p));
        kingdom.blue_leader = (self.leader_blue & map).iter().next().map(|p| (self.get_player(p), p));
        kingdom.green_leader = (self.leader_green & map).iter().next().map(|p| (self.get_player(p), p));
        kingdom.black_leader = (self.leader_black & map).iter().next().map(|p| (self.get_player(p), p));

        kingdom
    }

    /// find all the kingdoms on the board
    pub fn kingdoms(&self) -> Vec<Kingdom> {
        let mut visited = Bitboard::new();
        let mut kingdoms = vec![];
        for x in 0..H {
            for y in 0..W {
                let pos = pos!(x as u8, y as u8);
                if visited.get(pos) {
                    continue;
                }
                let kingdom = self.find_kingdom(pos, &mut visited);
                if kingdom.has_leader() {
                    kingdoms.push(kingdom);
                }
            }
        }
        kingdoms
    }

    fn available_treasures_count(&self) -> u8 {
        self.treasures.count_ones()
    }

    pub fn find_empty_spaces_adj_kingdom(&self, pos: Pos, is_river_space: bool) -> Vec<Pos> {
        let mut ret = Vec::new();
        let mut kingdom_map = Bitboard::new();

        self.find_kingdom_map(pos, &mut kingdom_map);

        // find empty spaces around kingdom
        for x in 0..H {
            for y in 0..W {
                if kingdom_map.get(pos!(x as u8, y as u8)) {
                    for neighbor in pos!(x as u8, y as u8).neighbors().iter() {
                        if self.get_tile_type(neighbor) == TileType::Empty && self.get_leader(neighbor) == Leader::None {
                            if is_river_space == RIVER.get(neighbor) {
                                ret.push(neighbor);
                            }
                        }
                    }
                }
            }
        }

        ret
    }

    fn find_disintegrable_tiles(&self, loser_pos: Pos, tile_type: TileType) -> Bitboard {
        // find all the tiles that are connected to the loser of TileType
        let mut ret = Bitboard::new();
        let mut visited = Bitboard::new();
        let mut stack = loser_pos.mask();
        while let Some(pos) = stack.pop() {
            if visited.get(pos) {
                continue;
            }
            visited.set(pos);

            if let Some(unification_tile) = self.unification_tile {
                if pos == unification_tile {
                    continue;
                }
            }

            // skip any red tiles that are next to a leader
            let nearby_leader = pos.neighbors().iter().any(|pos| {
                self.get_leader(pos) != Leader::None
            });
            let leader_near_red = self.get_tile_type(pos) == TileType::Red && nearby_leader;
            // can't destroy monument tiles
            if self.get_tile_type(pos) == tile_type
                && !leader_near_red
                && !self.get_monument(pos)
            {
                ret.set(pos);
            }

            if self.is_connectable(pos) {
                stack |= pos.neighbors()
            }
        }

        ret
    }

    #[inline(always)]
    pub fn get_monument(&self, pos: Pos) -> bool {
        self.monuments.get(pos)
    }

    #[inline(always)]
    pub fn set_monument(&mut self, pos: Pos, v: bool) {
        if v {
            self.monuments.set(pos)
        } else {
            self.monuments.reset(pos)
        }
    }


    #[inline(always)]
    pub fn get_treasure(&self, pos: Pos) -> bool {
        self.treasures.get(pos)
    }

    #[inline(always)]
    pub fn set_treasure(&mut self, pos: Pos, v: bool) {
        if v {
            self.treasures.set(pos)
        } else {
            self.treasures.reset(pos)
        }
    }

    #[inline(always)]
    pub fn set_catastrophe(&mut self, to: Pos, arg: bool) {
        if arg {
            self.catastrophes.set(to)
        } else {
            self.catastrophes.reset(to)
        }
    }

    #[inline(always)]
    pub fn get_catastrophe(&self, pos: Pos) -> bool {
        self.catastrophes.get(pos)
    }

    #[inline(always)]
    pub fn set_player(&mut self, pos: Pos, player: Player) {
        match player {
            Player::None => { self.player1.reset(pos); self.player2.reset(pos); },
            Player::Player1 => self.player1.set(pos),
            Player::Player2 => self.player2.set(pos),
        }
    }

    #[inline(always)]
    pub fn get_player(&self, from: Pos) -> Player {
        if self.player1.get(from) {
            Player::Player1
        } else if self.player2.get(from) {
            Player::Player2
        } else {
            Player::None
        }
    }

    #[inline(always)]
    pub fn get_leader(&self, pos: Pos) -> Leader {
        if self.leader_red.get(pos) {
            Leader::Red
        } else if self.leader_green.get(pos) {
            Leader::Green
        } else if self.leader_blue.get(pos) {
            Leader::Blue
        } else if self.leader_black.get(pos) {
            Leader::Black
        } else {
            Leader::None
        }
    }

    #[inline(always)]
    pub fn set_leader(&mut self, pos: Pos, leader: Leader) {
        match leader {
            Leader::None => {
                self.leader_red.reset(pos);
                self.leader_green.reset(pos);
                self.leader_blue.reset(pos);
                self.leader_black.reset(pos);
            }
            Leader::Red => self.leader_red.set(pos),
            Leader::Green => self.leader_green.set(pos),
            Leader::Blue => self.leader_blue.set(pos),
            Leader::Black => self.leader_black.set(pos),
        }
    }

    #[inline(always)]
    pub fn get_tile_type(&self, pos: Pos) -> TileType {
        if self.red_tiles.get(pos) {
            TileType::Red
        } else if self.green_tiles.get(pos) {
            TileType::Green
        } else if self.blue_tiles.get(pos) {
            TileType::Blue
        } else if self.black_tiles.get(pos) {
            TileType::Black
        } else {
            TileType::Empty
        }
    }

    #[inline(always)]
    pub fn set_tile_type(&mut self, pos: Pos, tile_type: TileType) {
        match tile_type {
            TileType::Empty => {
                self.red_tiles.reset(pos);
                self.green_tiles.reset(pos);
                self.blue_tiles.reset(pos);
                self.black_tiles.reset(pos);
            }
            TileType::Red => self.red_tiles.set(pos),
            TileType::Green => self.green_tiles.set(pos),
            TileType::Blue => self.blue_tiles.set(pos),
            TileType::Black => self.black_tiles.set(pos),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum MonumentType {
    RedGreen,
    RedBlue,
    GreenBlue,
    BlackRed,
    BlackGreen,
    BlackBlue,
}

impl MonumentType {
    pub fn matches(&self, color: TileType) -> bool {
        match (color, self) {
            (TileType::Red, MonumentType::BlackRed) => true,
            (TileType::Red, MonumentType::RedGreen) => true,
            (TileType::Red, MonumentType::RedBlue) => true,

            (TileType::Green, MonumentType::BlackGreen) => true,
            (TileType::Green, MonumentType::RedGreen) => true,
            (TileType::Green, MonumentType::GreenBlue) => true,

            (TileType::Blue, MonumentType::GreenBlue) => true,
            (TileType::Blue, MonumentType::RedBlue) => true,
            (TileType::Blue, MonumentType::BlackBlue) => true,

            (TileType::Black, MonumentType::BlackRed) => true,
            (TileType::Black, MonumentType::BlackGreen) => true,
            (TileType::Black, MonumentType::BlackBlue) => true,
            _ => false,
        }
    }

    fn unpack(&self) -> [Leader; 2] {
        match self {
            MonumentType::RedGreen => [Leader::Red, Leader::Green],
            MonumentType::RedBlue => [Leader::Red, Leader::Blue],
            MonumentType::GreenBlue => [Leader::Green, Leader::Blue],
            MonumentType::BlackRed => [Leader::Black, Leader::Red],
            MonumentType::BlackGreen => [Leader::Black, Leader::Green],
            MonumentType::BlackBlue => [Leader::Black, Leader::Blue],
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TileType {
    Empty,
    Blue,
    Green,
    Red,
    Black,
}

impl From<u8> for TileType {
    fn from(val: u8) -> Self {
        match val {
            0b0000 => TileType::Empty,
            0b0001 => TileType::Blue,
            0b0010 => TileType::Green,
            0b0100 => TileType::Red,
            0b1000 => TileType::Black,
            _ => panic!("invalid tile type"),
        }
    }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Player {
    None,
    Player1,
    Player2,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Leader {
    None,
    Blue,
    Green,
    Red,
    Black,
}

impl From<u8> for Leader {
    fn from(val: u8) -> Self {
        match val {
            0b0000 => Leader::None,
            0b0001 => Leader::Blue,
            0b0010 => Leader::Green,
            0b0100 => Leader::Red,
            0b1000 => Leader::Black,
            _ => panic!("invalid leader"),
        }
    }
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

pub type Result<T> = std::result::Result<T, Error>;

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
    MonumentNot2x2,
    CannotPlaceCatastropheOnMonument,
    CannotWithdrawLeader,
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! p1 {
        () => {
            (Player::Player1, PlayerAction::Normal)
        };
        ($i:expr) => {
            (Player::Player1, $i)
        };
    }

    macro_rules! p2 {
        () => {
            (Player::Player2, PlayerAction::Normal)
        };
        ($i:expr) => {
            (Player::Player2, $i)
        };
    }

    #[test]
    fn test_pos_neighbors() {
        let pos = pos!(0, 0);
        let neighbors = pos.neighbors().iter().collect::<Vec<_>>();
        assert_eq!(neighbors.len(), 2);
        assert!(neighbors.contains(&pos!(1, 0)));
        assert!(neighbors.contains(&pos!(0, 1)));
    }

    #[test]
    fn leader_must_be_placed_next_to_temples() {
        let mut game = TnEGame::new();
        assert_eq!(game.play_action_stack, vec![p1!(), p1!()]);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
        let ret = game.process(Action::PlaceLeader {
            pos: pos!(0, 0),
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
        let ret = game.process(Action::PlaceLeader {
            pos: pos!(0, 1),
            leader: Leader::Black,
        });
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Normal));
        assert_eq!(game.board.get_leader(pos!(0, 1)), Leader::Black);
        assert_eq!(game.board.get_player(pos!(0, 1)), Player::Player1);

        let ret = game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Normal));
        assert_eq!(game.board.get_leader(pos!(1, 0)), Leader::Red);
        assert_eq!(game.board.get_player(pos!(1, 0)), Player::Player1);
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
        game.process(Action::PlaceLeader {
            pos: pos!(0, 1),
            leader: Leader::Black,
        })
        .unwrap();
        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Red,
        })
        .unwrap();

        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);

        // player 2 puts down red leader
        assert_eq!(game.play_action_stack, vec![p2!(), p2!()]);
        let ret = game.process(Action::PlaceLeader {
            pos: pos!(2, 1),
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
                conflict_leader: Leader::Red,
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
        game.process(Action::PlaceLeader {
            pos: pos!(2, 1),
            leader: Leader::Red,
        })
        .unwrap_err();

        // player 2 tries to add 6 support, but fails
        assert_eq!(
            game.play_action_stack,
            vec![p2!(), p2!(PlayerAction::AddSupport(TileType::Red))]
        );
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Black);
        let ret = game.process(Action::AddSupport(6));
        assert_eq!(ret.unwrap_err(), Error::NotEnoughTiles);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);

        // player 2 successfully adds 1 support
        ensure_player_has_at_least_1_color(&mut game.players.0[1], TileType::Red);
        let ret = game.process(Action::AddSupport(1));
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
        let ret = game.process(Action::AddSupport(1));
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
        game.process(Action::PlaceLeader {
            pos: pos!(0, 1),
            leader: Leader::Red,
        })
        .unwrap();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        let ret = game.process(Action::PlaceTile {
            pos: pos!(0, 0),
            tile_type: TileType::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player2, PlayerAction::Normal));
        assert_eq!(game.board.get_tile_type(pos!(0, 0)), TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 1);
    }

    #[test]
    fn place_tile_no_leader() {
        let mut game = TnEGame::new();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        let ret = game.process(Action::PlaceTile {
            pos: pos!(0, 0),
            tile_type: TileType::Red,
        });
        assert_eq!(ret.unwrap(), (Player::Player1, PlayerAction::Normal));
        assert_eq!(game.board.get_tile_type(pos!(0, 0)), TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player2).hand_sum(), 6);
        assert_eq!(game.players.get_mut(Player::Player1).hand_sum(), 5);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 0);
    }

    #[test]
    fn place_tile_score_enemy_leader() {
        let mut game = TnEGame::new();
        game.process(Action::PlaceLeader {
            pos: pos!(0, 1),
            leader: Leader::Red,
        })
        .unwrap();
        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Black,
        })
        .unwrap();
        assert_eq!(game.play_action_stack, vec![p2!(), p2!()]);
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        game.process(Action::PlaceTile {
            pos: pos!(0, 0),
            tile_type: TileType::Red,
        })
        .unwrap();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Black);
        game.process(Action::PlaceTile {
            pos: pos!(2, 0),
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
        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Red,
        })
        .unwrap();
        game.process(Action::PlaceTile {
            pos: pos!(0, 2),
            tile_type: TileType::Red,
        })
        .unwrap();
        assert_eq!(game.board.get_tile_type(pos!(0, 2)), TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 0);

        // player 2 places a leader
        game.process(Action::PlaceLeader {
            pos: pos!(0, 3),
            leader: Leader::Red,
        })
        .unwrap();

        // player 2 places a tile joining two kingdoms
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        let ret = game
            .process(Action::PlaceTile {
                pos: pos!(0, 1),
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
                    conflict_leader: Leader::Red,
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
        game.process(Action::WarSelectLeader(Leader::Red))
        .unwrap();
        assert_eq!(
            game.play_action_stack,
            vec![(Player::Player2, PlayerAction::AddSupport(TileType::Red))]
        );

        // adds support
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        game.process(Action::AddSupport(1))
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
            .process(Action::AddSupport(1))
            .unwrap();
        assert_eq!(game.play_action_stack, vec![p1!(), p1!()]);
        assert_eq!(ret, (Player::Player1, PlayerAction::Normal));
    }

    #[test]
    fn place_tile_does_not_trigger_external_conflict() {
        let mut game = TnEGame::new();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Red,
        })
        .unwrap();
        game.process(Action::PlaceTile {
            pos: pos!(0, 2),
            tile_type: TileType::Red,
        })
        .unwrap();
        assert_eq!(game.board.get_tile_type(pos!(0, 2)), TileType::Red);
        assert_eq!(game.players.get_mut(Player::Player1).score_red, 0);
        game.process(Action::PlaceLeader {
            pos: pos!(0, 3),
            leader: Leader::Black,
        })
        .unwrap();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Red);
        let ret = game
            .process(Action::PlaceTile {
                pos: pos!(0, 1),
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
        game.process(Action::PlaceLeader {
            pos: pos!(0, 1),
            leader: Leader::Green,
        })
        .unwrap();
        let ret = game.board.find_empty_leader_space_next_to_red();
        assert_eq!(ret.iter().filter(|i| i.x < 5 && i.y < 5).count(), 3);
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Green);
        game.process(Action::PlaceTile {
            pos: pos!(1, 0),
            tile_type: TileType::Green,
        })
        .unwrap();
        assert_eq!(game.players.get_mut(Player::Player1).score_green, 1);
    }

    #[test]
    fn find_kingdom_adj() {
        let mut game = TnEGame::new();
        game.process(Action::PlaceLeader {
            pos: pos!(1, 2),
            leader: Leader::Green,
        })
        .unwrap();
        let ret = game.board.find_empty_spaces_adj_kingdom(pos!(1, 1), false);

        assert_eq!(
            ret,
            vec![
                pos!(0, 1),
                pos!(1, 0),
                pos!(2, 1),
                pos!(0, 2),
                pos!(1, 3),
                pos!(2, 2),
            ]
        );
    }

    #[test]
    fn external_conflict_resolution() {
        let mut game = TnEGame::new();
        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player1), TileType::Red);
        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Black,
        })
        .unwrap();
        game.process(Action::PlaceTile {
            pos: pos!(1, 3),
            tile_type: TileType::Red,
        })
        .unwrap();

        ensure_player_has_at_least_1_color(game.players.get_mut(Player::Player2), TileType::Black);
        game.process(Action::PlaceLeader {
            pos: pos!(0, 3),
            leader: Leader::Black,
        })
        .unwrap();
        game.process(Action::PlaceTile {
            pos: pos!(0, 2),
            tile_type: TileType::Black,
        })
        .unwrap();

        game.players.get_mut(Player::Player1).hand_black = 2;
        game.process(Action::PlaceTile {
            pos: pos!(1, 2),
            tile_type: TileType::Black,
        })
        .unwrap();
        game.process(Action::WarSelectLeader(Leader::Black))
        .unwrap();
        game.players.get_mut(Player::Player1).hand_black = 2;
        game.process(Action::AddSupport(2))
        .unwrap();

        game.process(Action::AddSupport(0))
        .unwrap();
        assert_eq!(game.board.get_tile_type(pos!(0, 2)), TileType::Empty);
        assert_eq!(game.next_state(), GameState::Normal);
    }

    #[test]
    fn test_monument() {
        let mut game = TnEGame::new();

        ensure_player_has_at_least_1_color(&mut game.players.0[0], TileType::Red);
        game.process(Action::PlaceTile {
            pos: pos!(0, 0),
            tile_type: TileType::Red,
        })
        .unwrap();
        ensure_player_has_at_least_1_color(&mut game.players.0[0], TileType::Red);
        game.process(Action::PlaceTile {
            pos: pos!(0, 1),
            tile_type: TileType::Red,
        })
        .unwrap();

        ensure_player_has_at_least_1_color(&mut game.players.0[1], TileType::Red);
        game.process(Action::PlaceTile {
            pos: pos!(1, 0),
            tile_type: TileType::Red,
        })
        .unwrap();
        assert_eq!(
            game.next(),
            (
                Player::Player2,
                PlayerAction::BuildMonument(
                    pos!(0, 0),
                    vec![
                        MonumentType::RedGreen,
                        MonumentType::RedBlue,
                        MonumentType::BlackRed
                    ]
                )
            )
        );
        game.process(Action::BuildMonument(MonumentType::RedGreen))
        .unwrap();

        ensure_player_has_at_least_1_color(&mut game.players.0[1], TileType::Red);
        game.process(Action::PlaceTile {
            pos: pos!(1, 2),
            tile_type: TileType::Red,
        })
        .unwrap();

        game.process(Action::PlaceLeader {
            pos: pos!(2, 2),
            leader: Leader::Green,
        })
        .unwrap();
        game.process(Action::Pass).unwrap();
        assert_eq!(game.players.0[0].score_green, 1);
    }

    #[test]
    fn test_neighbors() {
        let pos = pos!(1, 1);
        let mut n = Bitboard::new();
        n.set(pos!(0, 1));
        n.set(pos!(1, 0));
        n.set(pos!(1, 2));
        n.set(pos!(2, 1));
        assert!(pos.neighbors() == n);

        assert!(pos.neighbors().iter().count() == 4);
        assert!(pos!(0,3).neighbors().iter().count() == 3);
    }

    #[test]
    fn test_bug_leaders_disconnected() {
        let mut game = TnEGame::new();

        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Red,
        }).unwrap();
        game.process(Action::PlaceTile { pos: pos!(0, 3), tile_type: TileType::Red }).unwrap();

        game.process(Action::PlaceLeader { pos: pos!(1, 3), leader: Leader::Red }).unwrap();
        let count = game.board.nearby_kingdom_count();
        assert_eq!(count[1][2], 2);
    }

    #[test]
    fn test_find_kingdom() {
        let mut game = TnEGame::new();
        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Red,
        }).unwrap();

        let mut visited = Bitboard::new();
        let k = game.board.find_kingdom(pos!(1,0), &mut visited);
        assert!(k.map.get(pos!(1,0)));
        assert!(k.map.get(pos!(1,1)));
        assert_eq!(k.map.count_ones(), 2);
    }

    #[test]
    fn test_bitboard() {
        let mut bb = Bitboard::new();
        bb.set(pos!(0, 0));
        assert!(bb.get(pos!(0, 0)));
    }

    #[test]
    fn test_game_action_usize() {
        for i in 0..(11 * 16 * 11 + 6 + 7 + 4 + 1) {
            let action: Action = i.into();
            let ret: usize = action.into();
            assert_eq!(ret, i, "action: {:?}", action);
        }

        let action: Action = 124.into();
        dbg!(action);

    }

    #[test]
    fn test_generate_move() {
        let mut game = TnEGame::new();
        game.process(Action::PlaceLeader {
            pos: pos!(1, 0),
            leader: Leader::Red,
        }).unwrap();
        game.process(Action::PlaceTile { pos: pos!(0, 3), tile_type: TileType::Red }).unwrap();
        let moves = game.next_action().generate_moves(&game);
        dbg!(moves);
    }
}