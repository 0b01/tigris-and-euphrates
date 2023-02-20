#![allow(dead_code)]

use game::Pos;
use game::Action;
use game::Leader;
use game::Movement;
use game::TileType;
use history::HistoryBuffer;
use minimax::perft;
use minimax::Game;
use minimax::MCTSOptions;
use minimax::MonteCarloTreeSearch;
use minimax::Move;
use minimax::Negamax;
use minimax::Random;
use minimax::Strategy;

use crate::game::Player;
use crate::{
    game::TnEGame,
    solver::{Evaluator, TigrisAndEuphrates},
};

pub mod game;
mod history;
mod solver;
mod visualizer;

fn main() {
    let arg = std::env::args().nth(1).expect("no argument");

    match arg.as_str() {
        "play" => visualizer::play(TnEGame::new()),
        "view" => {
            let path = std::env::args().nth(2).expect("no path");
            view_history_json(&path);
        }
        "view_custom" => view_custom(),
        "test" => test_play(),
        "perft" => test_perft(),
        _ => panic!("unknown argument"),
    }
}

pub fn view_custom() {
    let mut game = HistoryBuffer::new();
    game.process(Action::PlaceTile {
        pos: pos!(0, 0),
        tile_type: TileType::Red,
    }).unwrap();
    visualizer::history_viewer(game.history);
}

pub fn view_history_json(path: &str) {
    let buf = HistoryBuffer::load(path).unwrap();
    visualizer::history_viewer(buf.history);
}

fn test_play() {
    let mut history = HistoryBuffer::new();

    let mut p1_strat = Negamax::new(Evaluator::default(), 2);
    let mut p2_strat = Negamax::new(Evaluator::default(), 2);

    // let mut p1_strat = MonteCarloTreeSearch::new(MCTSOptions::default().with_num_threads(1));
    // let mut p2_strat = MonteCarloTreeSearch::new(MCTSOptions::default().with_num_threads(1));

    // let mut p1_strat = Random::<TigrisAndEuphrates>::new();
    // let mut p2_strat = Random::<TigrisAndEuphrates>::new();

    while TigrisAndEuphrates::get_winner(history.last_mut()).is_none() {
        let curr_player = history.last_mut().next_player();
        let strategy = match curr_player {
            Player::Player1 => &mut p1_strat as &mut dyn Strategy<TigrisAndEuphrates>,
            Player::Player2 => &mut p2_strat as &mut dyn Strategy<TigrisAndEuphrates>,
            _ => unreachable!(),
        };
        match strategy.choose_move(&mut history.last_mut()) {
            Some(m) => {
                println!("{:?}: {:?}", curr_player, &m.move_);
                let ret = history.process(m.move_);
                if ret.is_err() {
                    history.save("out.json").unwrap();
                    ret.unwrap();
                }
            }
            None => {
                println!("move generator didn't generate any moves");
                break;
            }
        }
        // print!("[sum {}:{}], ", state.players.0[0].score_sum(), state.players.0[1].score_sum());
        println!(
            "[score {}:{}]",
            history.last_mut().players.0[0].calculate_score(),
            history.last_mut().players.0[1].calculate_score()
        );
    }
    // dbg!(history);
}

fn test_perft() {
    let mut state = TnEGame::new();
    perft::<TigrisAndEuphrates>(&mut state, 4, false);
}
