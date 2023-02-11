use crate::game::Game;

pub mod game;

fn main() {
    let g = Game::new();
    dbg!(g);
}
