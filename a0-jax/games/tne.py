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

    def __init__(self):
        super().__init__()
        self.reset()
        self.game = TnEGame()
        self.terminated = jnp.array(0, dtype=jnp.bool_)

    def num_actions(self) -> int:
        return TnEGame.N_ACTIONS

    def invalid_actions(self) -> chex.Array:
        return self.game.invalid_actions()

    def reset(self):
        self.game = TnEGame()
        self.who_play = jnp.array(1, dtype=jnp.int32)
        self.terminated = jnp.array(0, dtype=jnp.bool_)

    @pax.pure
    def step(self, action: chex.Array) -> Tuple["TnE", chex.Array]:
        """One step of the game.

        An invalid move will terminate the game with reward -1.
        """
        # get a true / false value for processing this action

        shape = jax.ShapeDtypeStruct(shape=(3,), dtype=jnp.float32)
        def _process(action):
            ret = jnp.array(self.game.process(action))
            if ret[0]:
                print(ret)
            return ret

        ret = jax.pure_callback(_process, shape, action, vectorized=False)
        success, reward, flip = ret[0], ret[1], ret[2]
        self.who_play = jnp.where(flip, -self.who_play, self.who_play)
        self.terminated = jnp.logical_or(self.terminated, reward != 0.0)
        reward = jnp.where(success, reward, -1.0) * self.who_play
        return self, reward

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
        np_array = jnp.array(np_array, dtype=jnp.float32)
        return np_array

    def is_terminated(self):
        return self.terminated

    def max_num_steps(self) -> int:
        return 1000

    def symmetries(self, state, action_weights):
        return [(state, action_weights), (np.flip(state), np.flip(action_weights))]

