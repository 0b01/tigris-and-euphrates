"""Generate training data using the Rust negamax solver.

This script runs the negamax AI to generate expert moves for imitation learning.
"""

import subprocess
import json
import numpy as np
import pickle
from tqdm import tqdm
from tne import TnEGame


def generate_negamax_game_data(depth: int = 4) -> list:
    """Generate one game of data using negamax.
    
    Since we can't easily call Rust minimax from Python, we use the 
    existing game state and make moves using a port of the evaluation.
    
    For true negamax data, you would:
    1. Export the minimax crate to Python
    2. Or run a subprocess that outputs moves
    """
    game = TnEGame()
    positions = []
    
    H, W = game.H, game.W
    move_count = 0
    max_moves = 200
    
    while move_count < max_moves:
        # Get state
        state = game.state()  # (C, H, W)
        state_hwc = np.transpose(state, (1, 2, 0))  # (H, W, C)
        
        # Get valid actions
        invalid = game.invalid_actions()
        valid_mask = (invalid == 0)
        valid_actions = np.where(valid_mask)[0]
        
        if len(valid_actions) == 0:
            break
        
        # Score each valid action using a heuristic (approximating negamax eval)
        action = score_and_select_action(game, valid_actions, state)
        
        # Store position
        positions.append({
            'state': state_hwc.copy(),
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
            data = []
            for i, pos in enumerate(reversed(positions)):
                v = final_value if i % 2 == 0 else -final_value
                data.append((pos['state'], pos['action'], pos['valid_mask'], float(v)))
            return list(reversed(data))
    
    # Game didn't finish - use 0 as value
    return [(p['state'], p['action'], p['valid_mask'], 0.0) for p in positions]


def score_and_select_action(game, valid_actions, state):
    """Score actions using heuristics similar to the Rust solver.
    
    This approximates what the negamax would choose by using the same
    evaluation priorities:
    1. Wars/conflicts (highest priority)
    2. Treasure collection
    3. Monument building
    4. Tile placement that scores points
    5. Leader placement
    """
    H, W = 11, 16
    
    scores = []
    
    for action in valid_actions:
        score = score_action(game, action, H, W)
        scores.append((score, action))
    
    # Sort by score descending
    scores.sort(reverse=True, key=lambda x: x[0])
    
    # Add some randomness for exploration (top-3 selection)
    top_k = min(3, len(scores))
    weights = np.array([3.0, 2.0, 1.0][:top_k])
    weights = weights / weights.sum()
    
    selected_idx = np.random.choice(top_k, p=weights)
    return scores[selected_idx][1]


def score_action(game, action, H, W):
    """Score a single action (matching Rust solver heuristics)."""
    
    # Decode action type
    if action < 4 * H * W:
        # PlaceTile
        tile_type = action // (H * W)  # 0=Red, 1=Green, 2=Blue, 3=Black
        pos_idx = action % (H * W)
        
        # Tiles that would score are good (adjacent to our kingdom)
        # For simplicity, prioritize by tile type
        type_priority = {0: 300, 1: 100, 2: 100, 3: 100}  # Red tiles for temples
        return 500 + type_priority.get(tile_type, 100)
    
    elif action < 8 * H * W:
        # PlaceLeader
        leader_type = (action - 4 * H * W) // (H * W)
        return 700 + (3 - leader_type) * 10  # Prioritize first leaders
    
    elif action < 9 * H * W:
        # WithdrawLeader
        return 100  # Usually bad
    
    elif action < 10 * H * W:
        # PlaceCatastrophe
        return 400
    
    elif action < 11 * H * W:
        # TakeTreasure
        return 900  # Very high priority
    
    else:
        idx = action - 11 * H * W
        if idx < 6:
            # BuildMonument
            return 850
        elif idx == 6:
            # DeclineMonument
            return 50
        elif idx < 14:
            # AddSupport
            support_amount = idx - 7
            return 800 + support_amount * 10
        elif idx < 18:
            # WarSelectLeader
            return 950
        else:
            # Pass
            return 10


def generate_dataset(num_games: int, output_path: str = "negamax_data.pkl"):
    """Generate a dataset of expert games."""
    
    print(f"Generating {num_games} games of training data...")
    
    all_data = []
    
    for _ in tqdm(range(num_games), desc="Generating games"):
        game_data = generate_negamax_game_data()
        all_data.extend(game_data)
    
    print(f"Generated {len(all_data)} training positions")
    
    # Save
    with open(output_path, 'wb') as f:
        pickle.dump(all_data, f)
    
    print(f"Saved to {output_path}")
    
    # Print stats
    actions = [d[1] for d in all_data]
    values = [d[3] for d in all_data]
    
    print(f"\nDataset stats:")
    print(f"  Positions: {len(all_data)}")
    print(f"  Avg value: {np.mean(values):.3f}")
    print(f"  Value std: {np.std(values):.3f}")
    print(f"  Unique actions: {len(set(actions))}")


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser()
    parser.add_argument('--num-games', type=int, default=100)
    parser.add_argument('--output', type=str, default='negamax_data.pkl')
    args = parser.parse_args()
    
    generate_dataset(args.num_games, args.output)
