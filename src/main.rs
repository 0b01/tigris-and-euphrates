use game::Board;

use crate::game::{Game, PlayerState};

pub mod game;

fn main() {
    let g = Game::new();
    dbg!(g);
}
