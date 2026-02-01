"""Imitation Learning: Train a neural network to mimic the negamax policy.

This is much faster than AlphaZero self-play because:
1. We use an expert (negamax) to generate training data
2. Supervised learning is more sample-efficient than RL
3. The network learns to approximate depth-4 search in a single forward pass

Usage:
    # Generate training data (run negamax games)
    python imitation_train.py --generate --num-games=100
    
    # Train the network
    python imitation_train.py --train --epochs=50
    
    # Evaluate against negamax
    python imitation_train.py --evaluate --num-games=20
"""

import os
import pickle
import numpy as np
import jax
import jax.numpy as jnp
import optax
from flax.training import train_state
from tqdm import tqdm
from typing import List, Tuple
import argparse

# Import the Rust game
from tne import TnEGame

# Import Flax policy
from policies.flax_policy import create_policy_value_network, init_network


class TrainState(train_state.TrainState):
    """Training state with batch stats for BatchNorm."""
    batch_stats: dict


def generate_negamax_data(num_games: int, depth: int = 4, save_path: str = "negamax_data.pkl") -> List[Tuple[np.ndarray, int, float]]:
    """Generate training data by running negamax self-play.
    
    Returns list of (state, action, value) tuples.
    """
    # We need to call the Rust negamax - let's use subprocess
    # For now, we'll generate data using a simple heuristic as placeholder
    # In production, you'd call the actual negamax
    
    print(f"Generating {num_games} games with negamax depth {depth}...")
    
    all_data = []
    
    for game_idx in tqdm(range(num_games), desc="Games"):
        game = TnEGame()
        game_positions = []
        
        move_count = 0
        max_moves = 200
        
        while move_count < max_moves:
            # Get state
            state = game.state()  # (C, H, W)
            state = np.transpose(state, (1, 2, 0))  # (H, W, C)
            
            # Get valid actions
            invalid = game.invalid_actions()
            valid_mask = (invalid == 0)
            valid_actions = np.where(valid_mask)[0]
            
            if len(valid_actions) == 0:
                break
            
            # For now, use a simple heuristic (prioritize leader placement, then tiles)
            # In production, this would call the actual negamax
            action = select_heuristic_action(game, valid_actions)
            
            # Store position
            game_positions.append({
                'state': state.copy(),
                'action': action,
                'valid_mask': valid_mask.copy(),
            })
            
            # Make move
            result = game.process(int(action))
            success, reward, flip = result
            
            if not success:
                break
            
            move_count += 1
            
            if reward != 0:
                # Game over - assign values
                final_value = reward
                for i, pos in enumerate(reversed(game_positions)):
                    v = final_value if i % 2 == 0 else -final_value
                    all_data.append((pos['state'], pos['action'], pos['valid_mask'], v))
                break
        else:
            # Game didn't finish - use 0 as value
            for pos in game_positions:
                all_data.append((pos['state'], pos['action'], pos['valid_mask'], 0.0))
    
    print(f"Generated {len(all_data)} training positions")
    
    # Save data
    with open(save_path, 'wb') as f:
        pickle.dump(all_data, f)
    print(f"Saved to {save_path}")
    
    return all_data


def select_heuristic_action(game, valid_actions):
    """Simple heuristic to select actions (placeholder for negamax).
    
    Priority:
    1. Take treasure
    2. Build monument  
    3. Place leader (if we have few on board)
    4. Place tile that scores
    5. Pass
    """
    H, W = 11, 16
    
    # Categorize actions
    treasures = []
    monuments = []
    leaders = []
    tiles = []
    other = []
    
    for a in valid_actions:
        if a >= 10 * H * W and a < 11 * H * W:  # TakeTreasure
            treasures.append(a)
        elif a >= 11 * H * W and a < 11 * H * W + 6:  # BuildMonument
            monuments.append(a)
        elif a >= 4 * H * W and a < 8 * H * W:  # PlaceLeader
            leaders.append(a)
        elif a < 4 * H * W:  # PlaceTile
            tiles.append(a)
        else:
            other.append(a)
    
    # Priority selection
    if treasures:
        return np.random.choice(treasures)
    if monuments:
        return np.random.choice(monuments)
    if leaders and np.random.random() < 0.7:  # 70% chance to place leader
        return np.random.choice(leaders)
    if tiles:
        return np.random.choice(tiles)
    if other:
        return np.random.choice(other)
    
    return valid_actions[0]


def load_data(path: str = "negamax_data.pkl"):
    """Load training data."""
    with open(path, 'rb') as f:
        return pickle.load(f)


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
def train_step(state, batch_states, batch_actions, batch_masks, batch_values):
    """Single training step for imitation learning."""
    
    def loss_fn(params):
        variables = {'params': params, 'batch_stats': state.batch_stats}
        result = state.apply_fn(
            variables, batch_states, train=True,
            mutable=['batch_stats']
        )
        (policy_logits, values), new_model_state = result
        
        # Mask invalid actions before computing loss
        masked_logits = jnp.where(batch_masks, policy_logits, -1e9)
        
        # Policy loss: cross-entropy with the expert action
        # Convert actions to one-hot
        num_actions = policy_logits.shape[-1]
        action_one_hot = jax.nn.one_hot(batch_actions, num_actions)
        
        log_probs = jax.nn.log_softmax(masked_logits)
        policy_loss = -jnp.sum(action_one_hot * log_probs, axis=-1)
        policy_loss = jnp.mean(policy_loss)
        
        # Value loss (MSE)
        value_loss = jnp.mean((values - batch_values) ** 2)
        
        # Combined loss (policy is more important for imitation)
        total_loss = policy_loss + 0.5 * value_loss
        
        return total_loss, (policy_loss, value_loss, new_model_state)
    
    grad_fn = jax.value_and_grad(loss_fn, has_aux=True)
    (loss, (policy_loss, value_loss, new_model_state)), grads = grad_fn(state.params)
    
    state = state.apply_gradients(grads=grads)
    if 'batch_stats' in new_model_state:
        state = state.replace(batch_stats=new_model_state['batch_stats'])
    
    return state, loss, policy_loss, value_loss


def train_imitation(
    data_path: str = "negamax_data.pkl",
    epochs: int = 50,
    batch_size: int = 64,
    learning_rate: float = 1e-3,
    num_filters: int = 64,
    num_blocks: int = 5,
    checkpoint_path: str = "imitation_model.pkl"
):
    """Train neural network to imitate negamax."""
    
    print("=" * 60)
    print("Imitation Learning for Tigris & Euphrates")
    print("=" * 60)
    print(f"Device: {jax.devices()[0]}")
    print(f"Network: {num_filters} filters, {num_blocks} blocks")
    print("=" * 60)
    
    # Load data
    print(f"Loading data from {data_path}...")
    data = load_data(data_path)
    print(f"Loaded {len(data)} positions")
    
    # Prepare arrays
    states = np.array([d[0] for d in data])
    actions = np.array([d[1] for d in data])
    masks = np.array([d[2] for d in data])
    values = np.array([d[3] for d in data])
    
    print(f"States shape: {states.shape}")
    print(f"Actions range: {actions.min()} - {actions.max()}")
    
    # Create model
    num_actions = 1955
    model = create_policy_value_network(num_actions, num_filters, num_blocks)
    
    # Initialize
    rng = jax.random.PRNGKey(42)
    train_st = create_train_state(rng, model, learning_rate)
    
    # Training loop
    num_batches = len(data) // batch_size
    
    for epoch in range(epochs):
        # Shuffle
        perm = np.random.permutation(len(data))
        
        total_loss = 0
        total_p_loss = 0
        total_v_loss = 0
        
        for i in range(num_batches):
            idx = perm[i * batch_size : (i + 1) * batch_size]
            
            batch_states = jnp.array(states[idx])
            batch_actions = jnp.array(actions[idx])
            batch_masks = jnp.array(masks[idx])
            batch_values = jnp.array(values[idx])
            
            train_st, loss, p_loss, v_loss = train_step(
                train_st, batch_states, batch_actions, batch_masks, batch_values
            )
            
            total_loss += loss
            total_p_loss += p_loss
            total_v_loss += v_loss
        
        avg_loss = total_loss / num_batches
        avg_p = total_p_loss / num_batches
        avg_v = total_v_loss / num_batches
        
        print(f"Epoch {epoch+1}/{epochs}: loss={avg_loss:.4f} (policy={avg_p:.4f}, value={avg_v:.4f})")
        
        # Save checkpoint every 10 epochs
        if (epoch + 1) % 10 == 0:
            save_checkpoint(train_st, checkpoint_path)
    
    # Final save
    save_checkpoint(train_st, checkpoint_path)
    print(f"\nTraining complete! Model saved to {checkpoint_path}")
    
    return train_st


def save_checkpoint(state, path):
    """Save model checkpoint."""
    with open(path, 'wb') as f:
        pickle.dump({
            'params': state.params,
            'batch_stats': state.batch_stats,
        }, f)


def load_checkpoint(path):
    """Load model checkpoint."""
    with open(path, 'rb') as f:
        return pickle.load(f)


def evaluate_model(
    checkpoint_path: str = "imitation_model.pkl",
    num_games: int = 20,
    num_filters: int = 64,
    num_blocks: int = 5,
):
    """Evaluate trained model against random baseline."""
    
    print("=" * 60)
    print("Evaluating Imitation Model")
    print("=" * 60)
    
    # Load model
    print(f"Loading model from {checkpoint_path}...")
    checkpoint = load_checkpoint(checkpoint_path)
    
    num_actions = 1955
    model = create_policy_value_network(num_actions, num_filters, num_blocks)
    
    variables = {
        'params': checkpoint['params'],
        'batch_stats': checkpoint['batch_stats']
    }
    
    # Play games: NN vs Random
    nn_wins = 0
    random_wins = 0
    draws = 0
    
    for game_idx in tqdm(range(num_games), desc="Eval games"):
        nn_is_p1 = game_idx % 2 == 0
        
        game = TnEGame()
        move_count = 0
        current_is_p1 = True
        
        while move_count < 200:
            invalid = game.invalid_actions()
            valid_actions = np.where(invalid == 0)[0]
            
            if len(valid_actions) == 0:
                break
            
            # Decide who plays
            nn_turn = (current_is_p1 and nn_is_p1) or (not current_is_p1 and not nn_is_p1)
            
            if nn_turn:
                # NN plays
                state = game.state()
                state = np.transpose(state, (1, 2, 0))[None]  # (1, H, W, C)
                
                policy_logits, value = model.apply(variables, jnp.array(state), train=False)
                policy_logits = np.array(policy_logits[0])
                
                # Mask and select
                policy_logits[invalid == 1] = -1e9
                action = int(np.argmax(policy_logits))
            else:
                # Random plays
                action = int(np.random.choice(valid_actions))
            
            result = game.process(action)
            success, reward, flip = result
            
            if not success:
                break
            
            if flip:
                current_is_p1 = not current_is_p1
            
            move_count += 1
            
            if reward != 0:
                if reward > 0:
                    winner_is_p1 = current_is_p1
                else:
                    winner_is_p1 = not current_is_p1
                
                if winner_is_p1 == nn_is_p1:
                    nn_wins += 1
                else:
                    random_wins += 1
                break
        else:
            draws += 1
    
    print(f"\nResults vs Random:")
    print(f"  NN wins: {nn_wins}")
    print(f"  Random wins: {random_wins}")
    print(f"  Draws: {draws}")
    print(f"  NN win rate: {nn_wins / num_games * 100:.1f}%")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Imitation Learning for T&E")
    parser.add_argument('--generate', action='store_true', help='Generate training data')
    parser.add_argument('--train', action='store_true', help='Train the model')
    parser.add_argument('--evaluate', action='store_true', help='Evaluate the model')
    parser.add_argument('--num-games', type=int, default=100, help='Number of games')
    parser.add_argument('--epochs', type=int, default=50, help='Training epochs')
    parser.add_argument('--batch-size', type=int, default=64, help='Batch size')
    parser.add_argument('--lr', type=float, default=1e-3, help='Learning rate')
    parser.add_argument('--depth', type=int, default=4, help='Negamax depth for data generation')
    parser.add_argument('--filters', type=int, default=64, help='Network filters')
    parser.add_argument('--blocks', type=int, default=5, help='Network blocks')
    parser.add_argument('--data-path', type=str, default='negamax_data.pkl')
    parser.add_argument('--checkpoint', type=str, default='imitation_model.pkl')
    
    args = parser.parse_args()
    
    if args.generate:
        generate_negamax_data(
            num_games=args.num_games,
            depth=args.depth,
            save_path=args.data_path
        )
    
    if args.train:
        train_imitation(
            data_path=args.data_path,
            epochs=args.epochs,
            batch_size=args.batch_size,
            learning_rate=args.lr,
            num_filters=args.filters,
            num_blocks=args.blocks,
            checkpoint_path=args.checkpoint
        )
    
    if args.evaluate:
        evaluate_model(
            checkpoint_path=args.checkpoint,
            num_games=args.num_games,
            num_filters=args.filters,
            num_blocks=args.blocks
        )
    
    if not (args.generate or args.train or args.evaluate):
        print("Usage:")
        print("  1. Generate data:  python imitation_train.py --generate --num-games=100")
        print("  2. Train model:    python imitation_train.py --train --epochs=50")
        print("  3. Evaluate:       python imitation_train.py --evaluate --num-games=20")
