use wasm_bindgen::prelude::*;
use serde::{Deserialize, Serialize};
use minimax::{Strategy, Negamax};

use crate::game::{
    Action, GameState, Leader, MonumentType, Player, PlayerAction, Pos, TileType, TnEGame, H, W,
};
use crate::solver::Evaluator;

/// JavaScript-friendly representation of the game state
#[derive(Serialize, Deserialize)]
pub struct JsGameState {
    pub board: JsBoard,
    pub player1: JsPlayerState,
    pub player2: JsPlayerState,
    pub current_player: u8, // 1 or 2
    pub game_state: String,
    pub next_action: JsPlayerAction,
    pub is_game_over: bool,
    pub winner: Option<u8>,
    pub available_monuments: Vec<String>,
    pub monuments: Vec<JsMonument>,
    pub internal_conflict: Option<JsConflict>,
    pub external_conflict: Option<JsExternalConflict>,
    pub bag_remaining: JsTiles,
}

#[derive(Serialize, Deserialize)]
pub struct JsBoard {
    pub tiles: Vec<Vec<JsTile>>,
    pub treasures: Vec<JsPos>,
}

#[derive(Serialize, Deserialize)]
pub struct JsTile {
    pub tile_type: String, // "empty", "red", "blue", "green", "black"
    pub leader: Option<String>, // "red", "blue", "green", "black"
    pub leader_owner: Option<u8>, // 1 or 2
    pub has_treasure: bool,
    pub is_catastrophe: bool,
    pub is_monument: bool,
}

#[derive(Serialize, Deserialize)]
pub struct JsPos {
    pub x: u8,
    pub y: u8,
}

#[derive(Serialize, Deserialize)]
pub struct JsPlayerState {
    pub hand_red: u8,
    pub hand_blue: u8,
    pub hand_green: u8,
    pub hand_black: u8,
    pub score_red: u8,
    pub score_blue: u8,
    pub score_green: u8,
    pub score_black: u8,
    pub score_treasure: u8,
    pub num_catastrophes: u8,
    pub leaders_on_board: Vec<String>,
    pub final_score: u8,
}

#[derive(Serialize, Deserialize)]
pub struct JsPlayerAction {
    pub action_type: String,
    pub options: Option<JsActionOptions>,
}

#[derive(Serialize, Deserialize)]
pub struct JsActionOptions {
    pub tile_type: Option<String>,
    pub treasures: Option<Vec<JsPos>>,
    pub leaders: Option<Vec<String>>,
    pub monuments: Option<Vec<String>>,
    pub monument_pos: Option<JsPos>,
}

#[derive(Serialize, Deserialize)]
pub struct JsMonument {
    pub monument_type: String,
    pub pos_top_left: JsPos,
}

#[derive(Serialize, Deserialize)]
pub struct JsConflict {
    pub is_internal: bool,
    pub attacker: u8,
    pub defender: u8,
    pub attacker_pos: JsPos,
    pub defender_pos: JsPos,
    pub conflict_leader: String,
    pub attacker_base_strength: u8,
    pub defender_base_strength: u8,
}

#[derive(Serialize, Deserialize)]
pub struct JsExternalConflict {
    pub conflicts: Vec<JsConflict>,
    pub unification_tile_pos: JsPos,
}

#[derive(Serialize, Deserialize)]
pub struct JsTiles {
    pub red: u8,
    pub blue: u8,
    pub green: u8,
    pub black: u8,
}

#[derive(Serialize, Deserialize)]
pub struct JsAction {
    pub action_type: String,
    pub pos: Option<JsPos>,
    pub tile_type: Option<String>,
    pub leader: Option<String>,
    pub support: Option<u8>,
    pub monument_type: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct JsMove {
    pub action: JsAction,
    pub notation: String,
}

/// Convert a TnEGame to a JavaScript-friendly representation
fn game_to_js(game: &TnEGame) -> JsGameState {
    let mut tiles = Vec::with_capacity(H);
    let mut treasures = Vec::new();
    
    for x in 0..H {
        let mut row = Vec::with_capacity(W);
        for y in 0..W {
            let pos = Pos { x: x as u8, y: y as u8 };
            let tile_type = match game.board.get_tile_type(pos) {
                TileType::Empty => "empty",
                TileType::Red => "red",
                TileType::Blue => "blue",
                TileType::Green => "green",
                TileType::Black => "black",
            }.to_string();
            
            let leader = match game.board.get_leader(pos) {
                Leader::None => None,
                Leader::Red => Some("red".to_string()),
                Leader::Blue => Some("blue".to_string()),
                Leader::Green => Some("green".to_string()),
                Leader::Black => Some("black".to_string()),
            };
            
            let leader_owner = if leader.is_some() {
                match game.board.get_player(pos) {
                    Player::Player1 => Some(1),
                    Player::Player2 => Some(2),
                    Player::None => None,
                }
            } else {
                None
            };
            
            let has_treasure = game.board.get_treasure(pos);
            if has_treasure {
                treasures.push(JsPos { x: x as u8, y: y as u8 });
            }
            
            row.push(JsTile {
                tile_type,
                leader,
                leader_owner,
                has_treasure,
                is_catastrophe: game.board.get_catastrophe(pos),
                is_monument: game.board.get_monument(pos),
            });
        }
        tiles.push(row);
    }
    
    let player_to_js = |player: Player| -> JsPlayerState {
        let ps = game.players.get(player);
        let mut leaders_on_board = Vec::new();
        for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black] {
            if ps.get_leader(leader).is_some() {
                leaders_on_board.push(match leader {
                    Leader::Red => "red",
                    Leader::Blue => "blue",
                    Leader::Green => "green",
                    Leader::Black => "black",
                    Leader::None => unreachable!(),
                }.to_string());
            }
        }
        JsPlayerState {
            hand_red: ps.hand_red,
            hand_blue: ps.hand_blue,
            hand_green: ps.hand_green,
            hand_black: ps.hand_black,
            score_red: ps.score_red,
            score_blue: ps.score_blue,
            score_green: ps.score_green,
            score_black: ps.score_black,
            score_treasure: ps.score_treasure,
            num_catastrophes: ps.num_catastrophes,
            leaders_on_board,
            final_score: ps.calculate_score(),
        }
    };
    
    let current_player = match game.next_player() {
        Player::Player1 => 1,
        Player::Player2 => 2,
        Player::None => 0,
    };
    
    let game_state = match game.next_state() {
        GameState::Normal => "normal",
        GameState::TakeTreasure => "take_treasure",
        GameState::AddSupport => "add_support",
        GameState::WarSelectLeader => "war_select_leader",
        GameState::BuildMonument => "build_monument",
    }.to_string();
    
    let next_action = match game.next_action() {
        PlayerAction::Normal => JsPlayerAction {
            action_type: "normal".to_string(),
            options: None,
        },
        PlayerAction::AddSupport(tile_type) => JsPlayerAction {
            action_type: "add_support".to_string(),
            options: Some(JsActionOptions {
                tile_type: Some(match tile_type {
                    TileType::Red => "red",
                    TileType::Blue => "blue",
                    TileType::Green => "green",
                    TileType::Black => "black",
                    _ => "empty",
                }.to_string()),
                treasures: None,
                leaders: None,
                monuments: None,
                monument_pos: None,
            }),
        },
        PlayerAction::SelectLeader { red, blue, green, black } => {
            let mut leaders = Vec::new();
            if red { leaders.push("red".to_string()); }
            if blue { leaders.push("blue".to_string()); }
            if green { leaders.push("green".to_string()); }
            if black { leaders.push("black".to_string()); }
            JsPlayerAction {
                action_type: "select_leader".to_string(),
                options: Some(JsActionOptions {
                    tile_type: None,
                    treasures: None,
                    leaders: Some(leaders),
                    monuments: None,
                    monument_pos: None,
                }),
            }
        },
        PlayerAction::TakeTreasure(positions) => JsPlayerAction {
            action_type: "take_treasure".to_string(),
            options: Some(JsActionOptions {
                tile_type: None,
                treasures: Some(positions.iter().map(|p| JsPos { x: p.x, y: p.y }).collect()),
                leaders: None,
                monuments: None,
                monument_pos: None,
            }),
        },
        PlayerAction::BuildMonument(pos, types) => JsPlayerAction {
            action_type: "build_monument".to_string(),
            options: Some(JsActionOptions {
                tile_type: None,
                treasures: None,
                leaders: None,
                monuments: Some(types.iter().map(|t| monument_type_to_string(*t)).collect()),
                monument_pos: Some(JsPos { x: pos.x, y: pos.y }),
            }),
        },
    };
    
    let is_game_over = game.state.is_empty();
    let winner = if is_game_over {
        match game.winner() {
            Player::Player1 => Some(1),
            Player::Player2 => Some(2),
            Player::None => None,
        }
    } else {
        None
    };
    
    let available_monuments: Vec<String> = game.available_monuments
        .iter()
        .map(|m| monument_type_to_string(*m))
        .collect();
    
    let monuments: Vec<JsMonument> = game.monuments
        .iter()
        .map(|m| JsMonument {
            monument_type: monument_type_to_string(m.monument_type),
            pos_top_left: JsPos { x: m.pos_top_left.x, y: m.pos_top_left.y },
        })
        .collect();
    
    let internal_conflict = game.internal_conflict.as_ref().map(|c| JsConflict {
        is_internal: true,
        attacker: match c.attacker {
            Player::Player1 => 1,
            Player::Player2 => 2,
            Player::None => 0,
        },
        defender: match c.defender {
            Player::Player1 => 1,
            Player::Player2 => 2,
            Player::None => 0,
        },
        attacker_pos: JsPos { x: c.attacker_pos.x, y: c.attacker_pos.y },
        defender_pos: JsPos { x: c.defender_pos.x, y: c.defender_pos.y },
        conflict_leader: leader_to_string(c.conflict_leader),
        attacker_base_strength: c.attacker_base_strength,
        defender_base_strength: c.defender_base_strength,
    });
    
    let external_conflict = game.external_conflict.as_ref().map(|ec| JsExternalConflict {
        conflicts: ec.conflicts.iter().map(|c| JsConflict {
            is_internal: false,
            attacker: match c.attacker {
                Player::Player1 => 1,
                Player::Player2 => 2,
                Player::None => 0,
            },
            defender: match c.defender {
                Player::Player1 => 1,
                Player::Player2 => 2,
                Player::None => 0,
            },
            attacker_pos: JsPos { x: c.attacker_pos.x, y: c.attacker_pos.y },
            defender_pos: JsPos { x: c.defender_pos.x, y: c.defender_pos.y },
            conflict_leader: leader_to_string(c.conflict_leader),
            attacker_base_strength: c.attacker_base_strength,
            defender_base_strength: c.defender_base_strength,
        }).collect(),
        unification_tile_pos: JsPos { x: ec.unification_tile_pos.x, y: ec.unification_tile_pos.y },
    });
    
    let bag = game.bag();
    let bag_remaining = JsTiles {
        red: bag.red,
        blue: bag.blue,
        green: bag.green,
        black: bag.black,
    };
    
    JsGameState {
        board: JsBoard { tiles, treasures },
        player1: player_to_js(Player::Player1),
        player2: player_to_js(Player::Player2),
        current_player,
        game_state,
        next_action,
        is_game_over,
        winner,
        available_monuments,
        monuments,
        internal_conflict,
        external_conflict,
        bag_remaining,
    }
}

fn monument_type_to_string(m: MonumentType) -> String {
    match m {
        MonumentType::RedGreen => "red_green",
        MonumentType::RedBlue => "red_blue",
        MonumentType::GreenBlue => "green_blue",
        MonumentType::BlackRed => "black_red",
        MonumentType::BlackGreen => "black_green",
        MonumentType::BlackBlue => "black_blue",
    }.to_string()
}

fn string_to_monument_type(s: &str) -> Option<MonumentType> {
    match s {
        "red_green" => Some(MonumentType::RedGreen),
        "red_blue" => Some(MonumentType::RedBlue),
        "green_blue" => Some(MonumentType::GreenBlue),
        "black_red" => Some(MonumentType::BlackRed),
        "black_green" => Some(MonumentType::BlackGreen),
        "black_blue" => Some(MonumentType::BlackBlue),
        _ => None,
    }
}

fn leader_to_string(l: Leader) -> String {
    match l {
        Leader::Red => "red",
        Leader::Blue => "blue",
        Leader::Green => "green",
        Leader::Black => "black",
        Leader::None => "none",
    }.to_string()
}

fn string_to_leader(s: &str) -> Leader {
    match s {
        "red" => Leader::Red,
        "blue" => Leader::Blue,
        "green" => Leader::Green,
        "black" => Leader::Black,
        _ => Leader::None,
    }
}

fn string_to_tile_type(s: &str) -> TileType {
    match s {
        "red" => TileType::Red,
        "blue" => TileType::Blue,
        "green" => TileType::Green,
        "black" => TileType::Black,
        _ => TileType::Empty,
    }
}

fn js_action_to_action(js: &JsAction) -> Option<Action> {
    match js.action_type.as_str() {
        "place_tile" => {
            let pos = js.pos.as_ref()?;
            let tile_type = string_to_tile_type(js.tile_type.as_ref()?);
            Some(Action::PlaceTile { pos: Pos { x: pos.x, y: pos.y }, tile_type })
        }
        "place_leader" => {
            let pos = js.pos.as_ref()?;
            let leader = string_to_leader(js.leader.as_ref()?);
            Some(Action::PlaceLeader { pos: Pos { x: pos.x, y: pos.y }, leader })
        }
        "withdraw_leader" => {
            let pos = js.pos.as_ref()?;
            Some(Action::WithdrawLeader(Pos { x: pos.x, y: pos.y }))
        }
        "place_catastrophe" => {
            let pos = js.pos.as_ref()?;
            Some(Action::PlaceCatastrophe(Pos { x: pos.x, y: pos.y }))
        }
        "take_treasure" => {
            let pos = js.pos.as_ref()?;
            Some(Action::TakeTreasure(Pos { x: pos.x, y: pos.y }))
        }
        "add_support" => {
            let support = js.support?;
            Some(Action::AddSupport(support))
        }
        "war_select_leader" => {
            let leader = string_to_leader(js.leader.as_ref()?);
            Some(Action::WarSelectLeader(leader))
        }
        "build_monument" => {
            let monument_type = string_to_monument_type(js.monument_type.as_ref()?)?;
            Some(Action::BuildMonument(monument_type))
        }
        "decline_monument" => {
            Some(Action::DeclineMonument)
        }
        "pass" => {
            Some(Action::Pass)
        }
        _ => None,
    }
}

/// The main game wrapper exposed to JavaScript
#[wasm_bindgen]
pub struct TnEWeb {
    game: TnEGame,
    ai_depth: u8,
}

#[wasm_bindgen]
impl TnEWeb {
    /// Create a new game
    #[wasm_bindgen(constructor)]
    pub fn new() -> TnEWeb {
        TnEWeb {
            game: TnEGame::new(),
            ai_depth: 5,
        }
    }

    /// Set the AI difficulty (search depth)
    #[wasm_bindgen]
    pub fn set_ai_depth(&mut self, depth: u8) {
        self.ai_depth = depth.max(1).min(6);
    }
    
    /// Get the AI difficulty (search depth)
    #[wasm_bindgen]
    pub fn get_ai_depth(&self) -> u8 {
        self.ai_depth
    }

    /// Get the current game state as JSON
    #[wasm_bindgen]
    pub fn get_state(&self) -> String {
        let js_state = game_to_js(&self.game);
        serde_json::to_string(&js_state).unwrap_or_else(|_| "{}".to_string())
    }

    /// Apply an action to the game, returns the new game state as JSON
    #[wasm_bindgen]
    pub fn apply_action(&mut self, action_json: &str) -> String {
        let js_action: Result<JsAction, _> = serde_json::from_str(action_json);
        match js_action {
            Ok(js) => {
                if let Some(action) = js_action_to_action(&js) {
                    match self.game.process(action) {
                        Ok(_) => self.get_state(),
                        Err(e) => format!("{{\"error\": \"Invalid action: {:?}\"}}", e),
                    }
                } else {
                    "{\"error\": \"Could not parse action\"}".to_string()
                }
            }
            Err(e) => format!("{{\"error\": \"JSON parse error: {}\"}}", e),
        }
    }

    /// Get all valid moves for the current player as JSON
    #[wasm_bindgen]
    pub fn get_valid_moves(&self) -> String {
        let action = self.game.next_action();
        let raw_moves = action.generate_moves(&self.game);
        
        let moves: Vec<JsMove> = raw_moves.iter().map(|a| {
            JsMove {
                action: action_to_js_action(a),
                notation: format!("{:?}", a),
            }
        }).collect();
        
        serde_json::to_string(&moves).unwrap_or_else(|_| "[]".to_string())
    }

    /// Get the AI's best move as JSON
    #[wasm_bindgen]
    pub fn get_ai_move(&mut self) -> String {
        let mut strategy = Negamax::new(Evaluator::default(), self.ai_depth);
        
        match strategy.choose_move(&self.game) {
            Some(m) => {
                let js_move = JsMove {
                    action: action_to_js_action(&m),
                    notation: format!("{:?}", m),
                };
                serde_json::to_string(&js_move).unwrap_or_else(|_| "{}".to_string())
            }
            None => "{\"error\": \"No valid moves\"}".to_string(),
        }
    }

    /// Let the AI make a move and return the new state
    #[wasm_bindgen]
    pub fn ai_play(&mut self) -> String {
        let mut strategy = Negamax::new(Evaluator::default(), self.ai_depth);
        
        match strategy.choose_move(&self.game) {
            Some(m) => {
                match self.game.process(m) {
                    Ok(_) => self.get_state(),
                    Err(e) => format!("{{\"error\": \"AI move failed: {:?}\"}}", e),
                }
            }
            None => "{\"error\": \"No valid moves available\"}".to_string(),
        }
    }

    /// Check if it's the AI's turn (player 2)
    #[wasm_bindgen]
    pub fn is_ai_turn(&self) -> bool {
        self.game.next_player() == Player::Player2 && !self.game.state.is_empty()
    }

    /// Check if the game is over
    #[wasm_bindgen]
    pub fn is_game_over(&self) -> bool {
        self.game.state.is_empty()
    }

    /// Get the winner (0 = draw/ongoing, 1 = player 1, 2 = player 2)
    #[wasm_bindgen]
    pub fn get_winner(&self) -> u8 {
        if !self.is_game_over() {
            return 0;
        }
        match self.game.winner() {
            Player::Player1 => 1,
            Player::Player2 => 2,
            Player::None => 0,
        }
    }

    /// Reset the game to initial state
    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.game = TnEGame::new();
    }
    
    /// Validate an action without applying it
    #[wasm_bindgen]
    pub fn validate_action(&self, action_json: &str) -> bool {
        let js_action: Result<JsAction, _> = serde_json::from_str(action_json);
        match js_action {
            Ok(js) => {
                if let Some(action) = js_action_to_action(&js) {
                    self.game.validate_action(action, self.game.next_state()).is_ok()
                } else {
                    false
                }
            }
            Err(_) => false,
        }
    }
}

fn action_to_js_action(action: &Action) -> JsAction {
    match action {
        Action::PlaceTile { pos, tile_type } => JsAction {
            action_type: "place_tile".to_string(),
            pos: Some(JsPos { x: pos.x, y: pos.y }),
            tile_type: Some(match tile_type {
                TileType::Red => "red",
                TileType::Blue => "blue",
                TileType::Green => "green",
                TileType::Black => "black",
                _ => "empty",
            }.to_string()),
            leader: None,
            support: None,
            monument_type: None,
        },
        Action::PlaceLeader { pos, leader } => JsAction {
            action_type: "place_leader".to_string(),
            pos: Some(JsPos { x: pos.x, y: pos.y }),
            tile_type: None,
            leader: Some(leader_to_string(*leader)),
            support: None,
            monument_type: None,
        },
        Action::WithdrawLeader(pos) => JsAction {
            action_type: "withdraw_leader".to_string(),
            pos: Some(JsPos { x: pos.x, y: pos.y }),
            tile_type: None,
            leader: None,
            support: None,
            monument_type: None,
        },
        Action::PlaceCatastrophe(pos) => JsAction {
            action_type: "place_catastrophe".to_string(),
            pos: Some(JsPos { x: pos.x, y: pos.y }),
            tile_type: None,
            leader: None,
            support: None,
            monument_type: None,
        },
        Action::TakeTreasure(pos) => JsAction {
            action_type: "take_treasure".to_string(),
            pos: Some(JsPos { x: pos.x, y: pos.y }),
            tile_type: None,
            leader: None,
            support: None,
            monument_type: None,
        },
        Action::AddSupport(n) => JsAction {
            action_type: "add_support".to_string(),
            pos: None,
            tile_type: None,
            leader: None,
            support: Some(*n),
            monument_type: None,
        },
        Action::WarSelectLeader(leader) => JsAction {
            action_type: "war_select_leader".to_string(),
            pos: None,
            tile_type: None,
            leader: Some(leader_to_string(*leader)),
            support: None,
            monument_type: None,
        },
        Action::BuildMonument(monument_type) => JsAction {
            action_type: "build_monument".to_string(),
            pos: None,
            tile_type: None,
            leader: None,
            support: None,
            monument_type: Some(monument_type_to_string(*monument_type)),
        },
        Action::DeclineMonument => JsAction {
            action_type: "decline_monument".to_string(),
            pos: None,
            tile_type: None,
            leader: None,
            support: None,
            monument_type: None,
        },
        Action::Pass => JsAction {
            action_type: "pass".to_string(),
            pos: None,
            tile_type: None,
            leader: None,
            support: None,
            monument_type: None,
        },
        Action::ReplaceTile(_) => JsAction {
            action_type: "replace_tile".to_string(),
            pos: None,
            tile_type: None,
            leader: None,
            support: None,
            monument_type: None,
        },
    }
}

/// Initialize panic hook for better error messages in browser console
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}
