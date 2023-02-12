use minimax::IterativeOptions;
use minimax::IterativeSearch;
use minimax::Negamax;
use minimax::ParallelOptions;
use minimax::ParallelSearch;
use minimax::Random;
use minimax::perft;
use minimax::Game;
use minimax::Move;
use minimax::Strategy;

use crate::game::Player;
use crate::{game::TnE, solver::{TigrisAndEuphrates, Evaluator}};

pub mod game;
mod solver;

fn main() {
    let mut state = TnE::new();

    let mut p1_strat = Negamax::new(Evaluator::default(), 4);
    let mut p2_strat = Negamax::new(Evaluator::default(), 2);
    // let mut p2_strat = Random::new();
    while TigrisAndEuphrates::get_winner(&state).is_none() {
        let curr_player = state.play_action_stack.last().unwrap().0;
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
            None => break,
        }
        println!("{}:{}", state.players.0[0].score_sum(), state.players.0[1].score_sum());
    }

    // perft::<TigrisAndEuphrates>(&mut state, 10, true);
}