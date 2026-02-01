#!/usr/bin/env python3
"""
Interactive mode for testing BGA bot locally
Run this to verify the Python bindings work correctly
"""

import tne

def action_to_string(idx: int, desc: str) -> str:
    """Convert action to human-readable format"""
    return f"[{idx}] {desc}"

def main():
    game = tne.TnEGame()
    
    print("=" * 60)
    print("Tigris & Euphrates - Interactive Test")
    print("=" * 60)
    
    while not game.is_game_over():
        print(f"\n{game.get_board_state()}")
        
        moves = game.get_legal_moves()
        print(f"\nLegal moves ({len(moves)}):")
        for i, (idx, desc) in enumerate(moves[:10]):  # Show first 10
            print(f"  {action_to_string(idx, desc)}")
        if len(moves) > 10:
            print(f"  ... and {len(moves) - 10} more")
        
        # Get AI suggestion
        print(f"\nPlayer {game.current_player()}'s turn")
        ai_move = game.get_ai_move(4)
        ai_desc = next((d for i, d in moves if i == ai_move), "Unknown")
        print(f"AI suggests: {action_to_string(ai_move, ai_desc)}")
        
        # Get user input
        user_input = input("\nEnter move index (or 'ai' for AI move, 'q' to quit): ").strip()
        
        if user_input.lower() == 'q':
            break
        elif user_input.lower() == 'ai':
            move_idx = ai_move
        else:
            try:
                move_idx = int(user_input)
            except ValueError:
                print("Invalid input")
                continue
        
        # Execute move
        result = game.process(move_idx)
        success, reward, flip = result
        if success:
            print(f"Move executed. Reward: {reward}, Turn changed: {bool(flip)}")
        else:
            print(f"Invalid move!")
    
    print("\n" + "=" * 60)
    print("GAME OVER")
    print(game.get_board_state())


if __name__ == "__main__":
    main()
