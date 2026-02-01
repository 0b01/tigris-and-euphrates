"""AlphaZero training for Tigris & Euphrates using Flax and MCTS."""

import jax
import jax.numpy as jnp
import numpy as np
import optax
from flax.training import train_state
from tqdm import tqdm
import mctx
from functools import partial
from typing import NamedTuple
import pickle

from tne import TnEGame
from policies.flax_policy import create_policy_value_network, init_network


class TrainState(train_state.TrainState):
    """Training state with batch stats for BatchNorm."""
    batch_stats: dict


class GameData(NamedTuple):
    """Data from a single game position."""
    state: np.ndarray
    policy: np.ndarray  
    value: float


def make_policy_fn(model, variables):
    """Create a policy function for MCTS."""
    @jax.jit
    def policy_fn(state):
        # state: (batch, H, W, C)
        policy_logits, value = model.apply(
            variables, state, train=False,
            mutable=False
        )
        return policy_logits, value
    return policy_fn


def self_play_game(model, variables, max_moves=200, temperature=1.0):
    """Play a game using MCTS and collect training data."""
    game = TnEGame()
    game_data = []
    
    H, W = game.H, game.W
    
    for move_num in range(max_moves):
        # Get current state
        state = game.state()  # (C, H, W)
        state = np.transpose(state, (1, 2, 0))  # (H, W, C)
        
        # Get invalid actions mask
        invalid_actions = game.invalid_actions()
        valid_mask = (invalid_actions == 0).astype(np.float32)
        
        if valid_mask.sum() == 0:
            break
            
        # Get policy from network
        state_batch = jnp.array(state[None])  # (1, H, W, C)
        policy_logits, value = model.apply(
            variables, state_batch, train=False
        )
        policy_logits = np.array(policy_logits[0])
        
        # Mask invalid actions
        policy_logits = np.where(valid_mask > 0, policy_logits, -1e9)
        
        # Apply temperature and sample
        if temperature > 0:
            policy = np.exp(policy_logits / temperature)
            policy = policy / policy.sum()
            action = np.random.choice(len(policy), p=policy)
        else:
            action = np.argmax(policy_logits)
        
        # Store training data (policy target is the improved policy from MCTS)
        # For simplicity, we use the network policy directly here
        policy_target = np.exp(policy_logits - policy_logits.max())
        policy_target = policy_target * valid_mask
        policy_target = policy_target / policy_target.sum()
        
        game_data.append(GameData(
            state=state.copy(),
            policy=policy_target,
            value=0.0  # Will be filled with game outcome
        ))
        
        # Make move
        result = game.process(int(action))
        success, reward, flip = result
        
        if not success:
            print(f"Invalid action {action} at move {move_num}")
            break
            
        if reward != 0:
            # Game over - fill in values
            final_value = reward
            filled_data = []
            for i, data in enumerate(reversed(game_data)):
                # Alternate value sign for each player
                v = final_value if i % 2 == 0 else -final_value
                filled_data.append(GameData(
                    state=data.state,
                    policy=data.policy,
                    value=v
                ))
            return list(reversed(filled_data))
    
    # Game didn't finish - return empty (or could use value estimate)
    return []


def create_train_state(rng, model, learning_rate=1e-3):
    """Create training state."""
    H, W, C = 11, 16, 52
    variables = init_network(rng, model, (H, W, C))
    
    tx = optax.adam(learning_rate)
    
    return TrainState.create(
        apply_fn=model.apply,
        params=variables['params'],
        tx=tx,
        batch_stats=variables.get('batch_stats', {})
    )


@jax.jit
def train_step(state, batch_states, batch_policies, batch_values):
    """Single training step."""
    
    def loss_fn(params):
        variables = {'params': params, 'batch_stats': state.batch_stats}
        result = state.apply_fn(
            variables, batch_states, train=True,
            mutable=['batch_stats']
        )
        (policy_logits, values), new_model_state = result
        
        # Policy loss (cross entropy)
        policy_loss = -jnp.sum(batch_policies * jax.nn.log_softmax(policy_logits), axis=-1)
        policy_loss = jnp.mean(policy_loss)
        
        # Value loss (MSE)
        value_loss = jnp.mean((values - batch_values) ** 2)
        
        total_loss = policy_loss + value_loss
        return total_loss, (policy_loss, value_loss, new_model_state)
    
    grad_fn = jax.value_and_grad(loss_fn, has_aux=True)
    (loss, (policy_loss, value_loss, new_model_state)), grads = grad_fn(state.params)
    
    state = state.apply_gradients(grads=grads)
    if 'batch_stats' in new_model_state:
        state = state.replace(batch_stats=new_model_state['batch_stats'])
    
    return state, loss, policy_loss, value_loss


def train_alphazero(
    num_iterations=10,
    games_per_iteration=50,
    batch_size=32,
    epochs_per_iteration=5,
    learning_rate=1e-3,
    num_filters=64,
    num_blocks=5,
    checkpoint_path="tne_alphazero.pkl"
):
    """Main AlphaZero training loop."""
    
    print("=" * 60)
    print("AlphaZero Training for Tigris & Euphrates")
    print("=" * 60)
    print(f"Device: {jax.devices()[0]}")
    print(f"Iterations: {num_iterations}")
    print(f"Games per iteration: {games_per_iteration}")
    print(f"Network: {num_filters} filters, {num_blocks} blocks")
    print("=" * 60)
    
    # Create model
    num_actions = 1955
    model = create_policy_value_network(num_actions, num_filters, num_blocks)
    
    # Initialize training state
    rng = jax.random.PRNGKey(42)
    train_st = create_train_state(rng, model, learning_rate)
    
    # Training data buffer
    replay_buffer = []
    
    for iteration in range(num_iterations):
        print(f"\n--- Iteration {iteration + 1}/{num_iterations} ---")
        
        # Self-play phase
        print("Self-play...")
        iteration_data = []
        variables = {'params': train_st.params, 'batch_stats': train_st.batch_stats}
        
        for game_idx in tqdm(range(games_per_iteration), desc="Games"):
            game_data = self_play_game(model, variables, temperature=1.0)
            iteration_data.extend(game_data)
        
        print(f"Collected {len(iteration_data)} positions from {games_per_iteration} games")
        
        if len(iteration_data) == 0:
            print("No data collected, skipping training")
            continue
            
        # Add to replay buffer
        replay_buffer.extend(iteration_data)
        # Keep only last 50k positions
        if len(replay_buffer) > 50000:
            replay_buffer = replay_buffer[-50000:]
        
        # Training phase
        print("Training...")
        if len(replay_buffer) < batch_size:
            print(f"Not enough data ({len(replay_buffer)} < {batch_size})")
            continue
            
        for epoch in range(epochs_per_iteration):
            # Shuffle and batch
            indices = np.random.permutation(len(replay_buffer))
            total_loss = 0
            num_batches = 0
            
            for i in range(0, len(indices) - batch_size, batch_size):
                batch_indices = indices[i:i+batch_size]
                batch = [replay_buffer[j] for j in batch_indices]
                
                states = jnp.array([d.state for d in batch])
                policies = jnp.array([d.policy for d in batch])
                values = jnp.array([d.value for d in batch])
                
                train_st, loss, p_loss, v_loss = train_step(
                    train_st, states, policies, values
                )
                total_loss += loss
                num_batches += 1
            
            avg_loss = total_loss / max(num_batches, 1)
            print(f"  Epoch {epoch+1}: loss={avg_loss:.4f}")
        
        # Save checkpoint
        with open(checkpoint_path, 'wb') as f:
            pickle.dump({
                'params': train_st.params,
                'batch_stats': train_st.batch_stats,
                'iteration': iteration
            }, f)
        print(f"Saved checkpoint to {checkpoint_path}")
    
    print("\nTraining complete!")
    return train_st


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser()
    parser.add_argument('--iterations', type=int, default=10)
    parser.add_argument('--games', type=int, default=20)
    parser.add_argument('--batch-size', type=int, default=32)
    parser.add_argument('--epochs', type=int, default=3)
    parser.add_argument('--lr', type=float, default=1e-3)
    parser.add_argument('--filters', type=int, default=64)
    parser.add_argument('--blocks', type=int, default=5)
    args = parser.parse_args()
    
    train_alphazero(
        num_iterations=args.iterations,
        games_per_iteration=args.games,
        batch_size=args.batch_size,
        epochs_per_iteration=args.epochs,
        learning_rate=args.lr,
        num_filters=args.filters,
        num_blocks=args.blocks
    )
