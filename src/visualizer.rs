use macroquad::{prelude::*, miniquad::conf::Icon};
use minimax::{Negamax, Strategy};

use crate::{pos, solver::Evaluator};
use crate::game::{TnEGame, Cell, Terrain, TileType, Leader, Player, W, H, Action, Pos, Movement, PlayerAction, Monument, MonumentType};

// the top left corner of the grid
const LOGICAL_GRID_START_Y: f32 = 8.;
const LOGICAL_GRID_START_X: f32 = 17.;

// logical size of the screen, it's the size of gameboy
const LOGICAL_W: f32 = 240.;
const LOGICAL_H: f32 = 160.;

// draw the hand at the right side of the screen
const HAND_X: f32 = 226.;
const HAND_Y: f32 = 10.;

fn physical_to_logical(v2: Vec2) -> Vec2 {
    Vec2 {
        y: v2.y * LOGICAL_H / screen_height(),
        x: v2.x * LOGICAL_W / screen_width(),
    }
}

fn logical_to_physical(v2: Vec2) -> Vec2 {
    Vec2 {
        x: v2.x * screen_height() / LOGICAL_H,
        y: v2.y * screen_width() / LOGICAL_W,
    }
}

fn grid_to_logical(pos: Pos) -> Vec2 {
    Vec2 {
        x: LOGICAL_GRID_START_X + pos.y as f32 * 13.,
        y: LOGICAL_GRID_START_Y + pos.x as f32 * 13.,
    }
}

fn logical_to_grid(pos: Vec2) -> Pos {
    let y = (pos.x - LOGICAL_GRID_START_X) / 13.;
    let x = (pos.y - LOGICAL_GRID_START_Y) / 13.;
    Pos { x: x as u8, y: y as u8 }
}

// if mouse is in the one of the grid cells
fn in_tile(pos: Vec2, tile: Vec2) -> bool {
    pos.x >= tile.x && pos.x <= tile.x + 12. && pos.y >= tile.y && pos.y <= tile.y + 12.
}

// holds all the textures
pub struct Textures {
    unification: Texture2D,
    treasure_blue: Texture2D,
    bull_blue: Texture2D,
    bull_green: Texture2D,
    bull_red: Texture2D,
    bull_black: Texture2D,
    vase_blue: Texture2D,
    vase_green: Texture2D,
    vase_red: Texture2D,
    vase_black: Texture2D,
    t_red: Texture2D,
    t_red_treasure: Texture2D,
    t_blue: Texture2D,
    t_black: Texture2D,
    t_green: Texture2D,
    circle: Texture2D,
    warning: Texture2D,

    monument_red_blue: Texture2D,
    monument_red_green: Texture2D,
    monument_green_blue: Texture2D,
    monument_black_red: Texture2D,
    monument_black_green: Texture2D,
    monument_black_blue: Texture2D,
}

impl Textures {
    async fn new() -> Self {
        let tiles = load_texture("assets/tiles.png").await.unwrap().get_texture_data();
        macro_rules! sub_img {
            ($i:expr) => {
                {
                    let t = Texture2D::from_image(
                        &tiles.sub_image(Rect { x: 12. * $i, y: 0., w: 12., h: 13. })
                    );
                    t.set_filter(FilterMode::Nearest);
                    t
                }
            };
            ($f:expr, $i:expr, $h:expr, $w:expr) => {
                {
                    let t = Texture2D::from_image(
                        &$f.sub_image(Rect { x: $w * $i, y: 0., w: $w, h: $h })
                    );
                    t.set_filter(FilterMode::Nearest);
                    t
                }
            }
        }

        let unification = sub_img!(0.);
        let treasure_blue = sub_img!(1.);
        let bull_blue = sub_img!(2.);
        let bull_green = sub_img!(3.);
        let bull_red = sub_img!(4.);
        let bull_black = sub_img!(5.);

        let vase_blue = sub_img!(6.);
        let vase_green = sub_img!(7.);
        let vase_red = sub_img!(8.);
        let vase_black = sub_img!(9.);

        let t_red = sub_img!(10.);
        let t_red_treasure = sub_img!(11.);
        let t_blue = sub_img!(12.);
        let t_black = sub_img!(13.);
        let t_green = sub_img!(14.);
        let circle = sub_img!(15.);
        let warning = sub_img!(16.);

        let monuments = load_texture("assets/monuments.png").await.unwrap().get_texture_data();
        let monument_red_blue = sub_img!(monuments, 0., 24., 25.);
        let monument_black_green = sub_img!(monuments, 1., 24., 25.);
        let monument_black_blue = sub_img!(monuments, 2., 24., 25.);
        let monument_black_red = sub_img!(monuments, 3., 24., 25.);
        let monument_green_blue = sub_img!(monuments, 4., 24., 25.);
        let monument_red_green = sub_img!(monuments, 5., 24., 25.);

        Self {
            monument_red_blue,
            monument_black_blue,
            monument_black_green,
            monument_black_red,
            monument_green_blue,
            monument_red_green,

            unification,
            treasure_blue,
            bull_blue,
            bull_green,
            bull_red,
            bull_black,
            vase_blue,
            vase_green,
            vase_red,
            vase_black,
            t_red,
            t_red_treasure,
            t_blue,
            t_black,
            t_green,
            circle,
            warning,
        }
    }

    fn draw_leader_logical(&self, leader: Leader, pos: Vec2) {
        let texture = match leader {
            Leader::Red => Some(self.bull_red),
            Leader::Blue => Some(self.bull_blue),
            Leader::Black => Some(self.bull_black),
            Leader::Green => Some(self.bull_green),
            _ => None,
        };
        if let Some(t) = texture {
            self.draw_texture_at_logical(t, pos);
        }
    }

    fn draw_tile_logical(&self, tile_type: TileType, pos: Vec2) {
        let texture = match tile_type {
            TileType::Red => Some(self.t_red),
            TileType::Blue => Some(self.t_blue),
            TileType::Black => Some(self.t_black),
            TileType::Green => Some(self.t_green),
            _ => None,
        };
        if let Some(t) = texture {
            self.draw_texture_at_logical(t, pos)
        }
    }

    fn draw_texture_at_logical(&self, texture: Texture2D, pos: Vec2) {
        let pos = logical_to_physical(pos - Vec2 { x: 0., y: 1. });
        let size = logical_to_physical(Vec2 { x: 12., y: 13. });
        draw_texture_ex(texture, pos.x, pos.y, WHITE, DrawTextureParams { dest_size: Some(size), ..Default::default()})
    }

    fn draw_texture_at_grid(&self, texture: Texture2D, pos: Pos) {
        let pos = logical_to_physical(grid_to_logical(pos) - Vec2 { x: 0., y: 1. });
        let size = logical_to_physical(Vec2 { x: 12., y: 13. });
        draw_texture_ex(texture, pos.x, pos.y, WHITE, DrawTextureParams { dest_size: Some(size.into()), ..Default::default()})
    }

    fn draw(&self, game: &TnEGame, cell: &Cell, pos: Pos) {
        let texture = if cell.terrain == Terrain::MonumentTopLeft {
            let m = game.monuments.iter().filter(|m|m.monument_top_left == pos).next().unwrap();
            let texture = match m.monument_type {
                MonumentType::RedBlue => self.monument_red_blue,
                MonumentType::BlackGreen => self.monument_black_green,
                MonumentType::BlackBlue => self.monument_black_blue,
                MonumentType::BlackRed => self.monument_black_red,
                MonumentType::GreenBlue => self.monument_green_blue,
                MonumentType::RedGreen => self.monument_red_green,
            };

            let pos = logical_to_physical(grid_to_logical(pos) - Vec2 { x: 0., y: 1. });
            let size = logical_to_physical(Vec2 { x: 25., y: 24. });
            draw_texture_ex(texture, pos.x, pos.y, WHITE, DrawTextureParams { dest_size: Some(size.into()), ..Default::default()});
            return;
        } else if cell.terrain == Terrain::Monument {
            None
        } else if cell.terrain == Terrain::Treasure {
            Some(self.t_red_treasure)
        } else if cell.tile_type == TileType::Red {
            Some(self.t_red)
        } else if cell.tile_type == TileType::Blue {
            Some(self.t_blue)
        } else if cell.tile_type == TileType::Black {
            Some(self.t_black)
        } else if cell.tile_type == TileType::Green {
            Some(self.t_green)
        } else {
            match (cell.leader, cell.player) {
                (Leader::Blue, Player::Player1) => Some(self.bull_blue),
                (Leader::Blue, Player::Player2) => Some(self.vase_blue),
                (Leader::Green, Player::Player1) => Some(self.bull_green),
                (Leader::Green, Player::Player2) => Some(self.vase_green),
                (Leader::Red, Player::Player1) => Some(self.bull_red),
                (Leader::Red, Player::Player2) => Some(self.vase_red),
                (Leader::Black, Player::Player1) => Some(self.bull_black),
                (Leader::Black, Player::Player2) => Some(self.vase_black),
                _ => None,
            }
        };
        if let Some(t) = texture {
            self.draw_texture_at_grid(t, pos);
        }
    }
}

struct Tile {
    logical_pos: Vec2,
    holding_type: Result<TileType, Leader>,
}

struct GameUIState {
    tiles: Vec<Tile>,
    selected: Option<usize>,
}

impl GameUIState {
    fn new(game: &TnEGame) -> Self {
        let mut s = Self {
            selected: None,
            tiles: vec![],
        };
        s.update_hand(game);
        s
    }

    fn draw(&self, textures: &Textures) {
        for tile in &self.tiles {
            match tile.holding_type {
                Ok(t) => textures.draw_tile_logical(t, tile.logical_pos),
                Err(l) => textures.draw_leader_logical(l, tile.logical_pos),
            }
        }
    }

    fn update_hand(&mut self, game: &TnEGame) {
        let player = &game.players.0[0];
        let hand = player.hand_to_vec();
        let mut hand: Vec<_> = hand.into_iter().map(Result::Ok).collect();
        
        for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].iter() {
            if player.get_leader(*leader).is_none() {
                hand.push(Err(*leader));
            }
        }

        self.tiles = hand.iter().enumerate().map(|(i, t)| Tile {
            logical_pos: vec2(HAND_X, HAND_Y + i as f32 * 12. + 2. * i as f32),
            holding_type: *t,
        }).collect();
    }
}

async fn run(mut game: TnEGame, draw_only: Option<&str>) {
    // // example monument:
    // game.board.0[0][0].terrain = Terrain::MonumentTopLeft;
    // game.monuments.push(Monument { monument_type: crate::game::MonumentType::BlackRed, monument_top_left: pos!(0,0) });

    let map = load_texture("assets/map.png").await.unwrap();
    map.set_filter(FilterMode::Nearest);
    let textures = Textures::new().await;
    let mut ui_state = GameUIState::new(&game);
    let mut ai_strategy = Negamax::new(Evaluator::default(), 3);
    // let mut strategy = Random::<TigrisAndEuphrates>::default();

    loop {
        ui_state.update_hand(&game);
        let mouse_logical = mouse_position_logical();
        clear_background(LIGHTGRAY);

        // draw the map
        draw_texture_ex(map, 0., 0., WHITE, DrawTextureParams {
            dest_size: Some(vec2(screen_width(), screen_height())),
            ..Default::default()
        });

        // draw internal conflict, behind tiles
        if let Some(conflict) = game.internal_conflict.as_ref() {
            textures.draw_texture_at_grid(textures.warning, conflict.attacker_pos);
            textures.draw_texture_at_grid(textures.warning, conflict.defender_pos);
        }

        // draw unification tile
        if let Some(unification_tile_pos) = game.external_conflict.as_ref().map(|i|i.unification_tile_pos) {
            textures.draw_texture_at_grid(textures.unification, unification_tile_pos);
        }

        // draw external conflict, a black circle surrounding leaders
        if let PlayerAction::SelectLeader { red, blue, green, black } = game.next_action() {
            for leader in [Leader::Red, Leader::Blue, Leader::Green, Leader::Black].into_iter() {
                let in_conflict = match leader {
                    Leader::Red => red,
                    Leader::Blue => blue,
                    Leader::Green => green,
                    Leader::Black => black,
                    _ => unreachable!(),
                };
                if !in_conflict {
                    continue;
                }

                if let Some(pos) = game.players.get_mut(Player::Player1).get_leader(leader) {
                    textures.draw_texture_at_grid(textures.circle, pos);
                    if is_mouse_button_pressed(MouseButton::Left) && in_tile(mouse_logical, grid_to_logical(pos)) {
                        process(&mut game, Action::WarSelectLeader { leader });
                    }
                }
            }
        }

        // draw the tiles
        for (i, row) in game.board.0.iter().enumerate() {
            for (j, cell) in row.iter().enumerate() {
                textures.draw(&game, cell, pos!(i as u8, j as u8));
            }
        }

        // if we can take treasure, draw treasure tiles
        if let PlayerAction::TakeTreasure(ts) = game.next_action() {
            for t in ts {
                textures.draw_texture_at_grid(textures.treasure_blue, t);

                // if clicked on those tiles, take the treasure
                if logical_to_grid(mouse_logical) == t && is_mouse_button_pressed(MouseButton::Left) {
                    process(&mut game, Action::TakeTreasure(t));
                    ui_state.update_hand(&game);
                }
            }
        }

        if is_key_pressed(KeyCode::P) {
            process(&mut game, Action::Pass);
        }

        // if we can add support, check if we pressed a number key
        let n = if is_key_pressed(KeyCode::Key0) {
            Some(0)
        } else if is_key_pressed(KeyCode::Key1) {
            Some(1)
        } else if is_key_pressed(KeyCode::Key2) {
            Some(2)
        } else if is_key_pressed(KeyCode::Key3) {
            Some(3)
        } else if is_key_pressed(KeyCode::Key4) {
            Some(4)
        } else if is_key_pressed(KeyCode::Key5) {
            Some(5)
        } else if is_key_pressed(KeyCode::Key6) {
            Some(6)
        } else {
            None
        };
        if let Some(n) = n {
            if let PlayerAction::AddSupport(tile_type) = game.next_action() {
                process(&mut game, Action::AddSupport { tile_type, n });
            }
        }

        // draw tile if we are dragging
        ui_state.draw(&textures);

        // if clicked on hand or grid
        if is_mouse_button_pressed(MouseButton::Left) {
            // if we are holding a tile
            if let Some(i) = ui_state.selected {
                let holding_type = ui_state.tiles[i].holding_type;
                if in_grid(mouse_logical) {
                    let pos = logical_to_grid(mouse_logical);

                    let action = match holding_type {
                        Ok(tile_type) => Action::PlaceTile { to: pos, tile_type },
                        Err(leader) => Action::MoveLeader { movement: Movement::Place(pos), leader},
                    };

                    if let Ok(()) = game.validate_action(action, game.next_state()) {
                        process(&mut game, action);
                    }
                }

                ui_state.selected = None;
            }

            for (i, Tile {logical_pos: pos, ..}) in ui_state.tiles.iter_mut().enumerate() {
                if in_tile(mouse_logical, *pos) {
                    ui_state.selected = Some(i); 
                    break;
                }
            }
        }

        // if we are holding a tile
        if let Some(i) = ui_state.selected {
            let holding_type = ui_state.tiles[i].holding_type;

            // if clicked in the grid
            let pos = if in_grid(mouse_logical) {
                // snap to grid
                let x = (mouse_logical.x - LOGICAL_GRID_START_X) / 13.;
                let grid_x = x.floor();
                let x = grid_x * 13. + LOGICAL_GRID_START_X;
                let y = (mouse_logical.y - LOGICAL_GRID_START_Y) / 13.;
                let grid_y = y.floor();
                let y = grid_y * 13. + LOGICAL_GRID_START_Y;

                let action = match holding_type {
                    Ok(tile_type) => Action::PlaceTile { to: pos!(grid_y as u8, grid_x as u8), tile_type },
                    Err(leader) => Action::MoveLeader { movement: Movement::Place(pos!(grid_y as u8, grid_x as u8)), leader},
                };
                match game.validate_action(action, game.next_state()) {
                    Ok(_) => Some(vec2(x, y)),
                    Err(_) => None,
                }
            } else {
                Some(mouse_logical)
            };

            if let Some(pos) = pos {
                match holding_type {
                    Ok(t) => textures.draw_tile_logical(t, pos),
                    Err(l) => textures.draw_leader_logical(l, pos),
                }
            }
        }

        // finish drawing
        next_frame().await;

        if let Some(path) = draw_only {
            screenshot(path);
            break;
        }

        if is_key_pressed(KeyCode::S) {
            screenshot("screenshot.png");
        }

        // if the next player is the AI, let it play, could take a while
        if game.next_player() == Player::Player2 {
            let m = ai_strategy.choose_move(&mut game);
            if m.is_none() {
                // take a screenshot
                screenshot("no_move.png");
                return;
            }

            let m = m.unwrap();
            process(&mut game, m.move_);

            let calculate_score = |p| {
                let player_state = game.players.get_mut(p);
                (player_state.get_eval(&game), player_state.calculate_score())
            };

            let (e1, s1) = calculate_score(Player::Player1);
            let (e2, s2) = calculate_score(Player::Player2);
            let player_state = game.players.get_mut(Player::Player1);
            print!("\t[Score: {} - {}]", s1, s2);
            print!("[r({}) - black({}) - blue({}) - g({})]", player_state.score_red, player_state.score_black, player_state.score_blue, player_state.score_green);
            println!("[Eval: {} - {}]", e1, e2);
        }

        if game.state.is_empty() {
            // game is over 
            screenshot("gameover.png");
            return;
        }
    }
}

fn screenshot(filename: &str) {
    get_screen_data().export_png(filename);
}

fn mouse_position_logical() -> Vec2 {
    let (x, y) = mouse_position();
    let mouse_logical = physical_to_logical(vec2(x, y));
    mouse_logical
}

fn in_grid(Vec2{x,y}: Vec2) -> bool {
    x > LOGICAL_GRID_START_X && x < LOGICAL_GRID_START_X + 12. * W as f32 + W as f32
    && y > LOGICAL_GRID_START_Y && y < LOGICAL_GRID_START_Y + 12. * H as f32 + H as f32
}

pub fn start(game: TnEGame) {
    let small = Image::from_file_with_format(include_bytes!("../assets/icon_small.png"), None).bytes.leak().try_into().unwrap();
    let medium = Image::from_file_with_format(include_bytes!("../assets/icon_medium.png"), None).bytes.leak().try_into().unwrap();
    let big = Image::from_file_with_format(include_bytes!("../assets/icon_big.png"), None).bytes.leak().try_into().unwrap();
    let icon = Icon {
        small,
        medium,
        big
    };

    macroquad::Window::from_config(Conf {
        window_title: "Tigris and Euphrates".to_string(),
        window_width: 240 * 5,
        window_height: 160 * 5,
        high_dpi: false,
        window_resizable: false,
        icon: Some(icon),
        ..Default::default()
    }, run(game, None));
}

fn process(game: &mut TnEGame, action: Action) {
    let ret = game.process(action);
    if let Ok(_) = ret {
        println!("{:?}: {:?}", game.last_action_player, &action);
    } else {
        dbg!(ret.unwrap_err());
    }
}