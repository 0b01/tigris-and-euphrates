"""TnEGame"""

from typing import Tuple

import chex
import jax.numpy as jnp
import numpy as np
import pax
import jax

from games.env import Enviroment
from utils import select_tree
from tne import TnEGame

class TnE(Enviroment):
    """TnE game environment"""

    game: "TnEGame"
    who_play: chex.Array
    count: chex.Array
    terminated: chex.Array
    winner: chex.Array

    def __init__(self):
        super().__init__()
        self.reset()
        # self.winner_checker = Connect2WinChecker()
        self.game = TnEGame()
        self.terminated = jnp.array(0, dtype=jnp.bool_)
        self.winner = jnp.array(0, dtype=jnp.int32)

    def num_actions(self) -> int:
        return TnEGame.N_ACTIONS

    def invalid_actions(self) -> chex.Array:
        return self.game.invalid_actions()

    def reset(self):
        self.game = TnEGame()
        self.who_play = jnp.array(1, dtype=jnp.int32)
        self.count = jnp.array(0, dtype=jnp.int32)
        self.terminated = jnp.array(0, dtype=jnp.bool_)
        self.winner = jnp.array(0, dtype=jnp.int32)

    @pax.pure
    def step(self, action: chex.Array) -> Tuple["TnE", chex.Array]:
        """One step of the game.

        An invalid move will terminate the game with reward -1.
        """
        # get a true / false value for processing this action

        shape = jax.ShapeDtypeStruct(shape=(), dtype=jnp.bool_) # a boolean value
        def _process(action):
            return jnp.array(self.game.process(action))

        is_valid = jax.pure_callback(_process, shape, action, vectorized=False)
        reward = is_valid
        return self, reward
        # invalid_move = self.board[action] != 0
        # board_ = self.board.at[action].set(self.who_play)
        # self.board = select_tree(self.terminated, self.board, board_)
        # # self.winner = self.winner_checker(self.board)
        # reward = self.winner * self.who_play
        # self.who_play = -self.who_play
        # self.count = self.count + 1
        # self.terminated = jnp.logical_or(self.terminated, reward != 0)
        # self.terminated = jnp.logical_or(self.terminated, self.count >= 4)
        # self.terminated = jnp.logical_or(self.terminated, invalid_move)
        # reward = jnp.where(invalid_move, -1.0, reward)
        # return self, reward

    def render(self) -> None:
        """Render the game on screen."""
        # print("0 1 2 3")
        # N = len(self.board)
        # for i in range(N):
        #     if self.board[i].item() == 1:
        #         print("X", end=" ")
        #     elif self.board[i].item() == -1:
        #         print("O", end=" ")
        #     else:
        #         print(".", end=" ")
        print()

    def observation(self) -> chex.Array:
        np_array = self.game.state()
        return jnp.array(np_array, dtype=jnp.float32)

    def canonical_observation(self) -> chex.Array:
        np_array = self.game.state()
        return jnp.array(np_array, dtype=jnp.float32)

    def is_terminated(self):
        return self.terminated

    def max_num_steps(self) -> int:
        return 4

    def symmetries(self, state, action_weights):
        return [(state, action_weights), (np.flip(state), np.flip(action_weights))]

