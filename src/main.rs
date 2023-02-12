use game::Action;
use game::Leader;
use game::Movement;
use game::TileType;
use game::pos;
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

    // some preset scenarios for debugging: 
    state.process(Action::MoveLeader { movement: Movement::Place(pos!("1B")), leader: Leader::Red }).unwrap();
    state.process(Action::PlaceTile { to: pos!("1D"), tile_type: TileType::Red }).unwrap();
    state.process(Action::MoveLeader { movement: Movement::Place(pos!("2D")), leader: Leader::Red }).unwrap();

    let mut p1_strat = Negamax::new(Evaluator::default(), 4);
    let mut p2_strat = Random::new();
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
                dbg!();
                continue;
            }
        }
        println!("{}:{}", state.players.0[0].score_sum(), state.players.0[1].score_sum());
    }

    dbg!(state);

    // perft::<TigrisAndEuphrates>(&mut state, 10, true);
}