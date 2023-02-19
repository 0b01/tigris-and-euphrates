#![allow(dead_code)]

use game::pos;
use game::Action;
use game::Leader;
use game::Movement;
use game::TileType;
use history::HistoryBuffer;
use minimax::MCTSOptions;
use minimax::MonteCarloTreeSearch;
use minimax::Random;
use minimax::perft;
use minimax::Game;
use minimax::Move;
use minimax::Negamax;
use minimax::Strategy;

use crate::game::Player;
use crate::{
    game::TnEGame,
    solver::{Evaluator, TigrisAndEuphrates},
};

pub mod game;
mod solver;
mod visualizer;
mod history;

fn main() {
    // visualizer::play(TnEGame::new());
    // test_play();
    test_play_history();
}

pub fn test_play_history() {
    let mut buf = HistoryBuffer::new();
    buf.push(TnEGame::new());
    buf
        .process(Action::MoveLeader {
            movement: Movement::Place(pos!("1B")),
            leader: Leader::Red,
        })
        .unwrap();
    buf
        .process(Action::PlaceTile {
            to: pos!("1D"),
            tile_type: TileType::Red,
        })
        .unwrap();
    buf
        .process(Action::MoveLeader {
            movement: Movement::Place(pos!("2D")),
            leader: Leader::Red,
        })
        .unwrap();

    visualizer::history_viewer(buf.history);
}


fn test_play() {
    let mut state = TnEGame::new();

    // // some preset scenarios for debugging:
    state
        .process(Action::MoveLeader {
            movement: Movement::Place(pos!("1B")),
            leader: Leader::Red,
        })
        .unwrap();
    state
        .process(Action::PlaceTile {
            to: pos!("1D"),
            tile_type: TileType::Red,
        })
        .unwrap();
    state
        .process(Action::MoveLeader {
            movement: Movement::Place(pos!("2D")),
            leader: Leader::Red,
        })
        .unwrap();

    // let mut p1_strat = Negamax::new(Evaluator::default(), 2);
    // let mut p2_strat = Negamax::new(Evaluator::default(), 2);

    let mut p1_strat = MonteCarloTreeSearch::new(
        MCTSOptions::default()
            .with_num_threads(1)
    );
    // let mut p2_strat = MonteCarloTreeSearch::new(
    //     MCTSOptions::default()
    //         // .with_num_threads(1)
    // );

    // let mut p1_strat = Random::<TigrisAndEuphrates>::new();
    let mut p2_strat = Random::<TigrisAndEuphrates>::new();

    // let mut p2_strat = Random::new();
    while TigrisAndEuphrates::get_winner(&state).is_none() {
        let curr_player = state.next_player();
        let strategy = match curr_player {
            Player::Player1 => &mut p1_strat as &mut dyn Strategy<TigrisAndEuphrates>,
            Player::Player2 => &mut p2_strat as &mut dyn Strategy<TigrisAndEuphrates>,
            _ => unreachable!(),
        };
        match strategy.choose_move(&mut state) {
            Some(m) => {
                println!("{:?}: {:?}", curr_player, &m.move_);
                m.apply(&mut state);
            }
            None => {
                println!("move generator didn't generate any moves");
                break;
            }
        }
        // print!("[sum {}:{}], ", state.players.0[0].score_sum(), state.players.0[1].score_sum());
        println!(
            "[score {}:{}]",
            state.players.0[0].calculate_score(),
            state.players.0[1].calculate_score()
        );
    }
    dbg!(state);
}

fn test_perft() {
    let mut state = TnEGame::new();
    perft::<TigrisAndEuphrates>(&mut state, 10, true);
}
