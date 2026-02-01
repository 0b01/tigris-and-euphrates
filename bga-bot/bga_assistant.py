#!/usr/bin/env python3
"""
BGA Assistant for Tigris & Euphrates

This is a semi-automated assistant that:
1. Maintains game state internally
2. Suggests moves via AI
3. User manually executes moves on BGA
4. User inputs opponent's moves to keep state in sync

Usage:
1. Start a BGA game
2. Run this script
3. Enter moves as they happen
4. AI will suggest your moves
"""

import re
import tne

# Position mapping: BGA uses letter-number (e.g., "A1" = column A, row 1)
# Our internal format uses Pos { x, y } where x=row, y=column

def parse_position(pos_str: str) -> tuple:
    """Parse position like '1P' or 'A1' to (x, y) where x=row, y=col"""
    pos_str = pos_str.upper().strip()
    
    # Try number-letter format first (1P = row 0, col 15) - matches internal format
    match = re.match(r'(\d+)([A-P])', pos_str)
    if match:
        row = int(match.group(1)) - 1  # x = row, 0-indexed
        col = ord(match.group(2)) - ord('A')  # y = col
        return (row, col)
    
    # Also accept letter-number format (P1 = col 15, row 0)
    match = re.match(r'([A-P])(\d+)', pos_str)
    if match:
        col = ord(match.group(1)) - ord('A')
        row = int(match.group(2)) - 1
        return (row, col)
    
    return None

def pos_to_string(x: int, y: int) -> str:
    """Convert internal position to display format (row+col like 1P)"""
    col = chr(ord('A') + y)
    row = x + 1
    return f"{row}{col}"

def parse_tile_type(t: str) -> str:
    """Parse tile type"""
    t = t.lower().strip()
    if t in ['r', 'red', 'temple']:
        return 'Red'
    elif t in ['g', 'green', 'market']:
        return 'Green'
    elif t in ['b', 'blue', 'farm']:
        return 'Blue'
    elif t in ['k', 'black', 'settlement']:
        return 'Black'
    return None

def parse_leader(l: str) -> str:
    """Parse leader type"""
    l = l.lower().strip()
    if l in ['r', 'red', 'priest']:
        return 'Red'
    elif l in ['g', 'green', 'trader']:
        return 'Green'
    elif l in ['b', 'blue', 'farmer']:
        return 'Blue'
    elif l in ['k', 'black', 'king']:
        return 'Black'
    return None

def find_matching_move(game, action_type: str, **kwargs) -> int:
    """Find a move matching the given criteria"""
    moves = game.get_legal_moves()
    
    for idx, desc in moves:
        if action_type not in desc:
            continue
        
        # Check position match if specified
        if 'pos' in kwargs:
            pos_str = f"x: {kwargs['pos'][0]}, y: {kwargs['pos'][1]}"
            if pos_str not in desc:
                continue
        
        # Check tile type if specified
        if 'tile_type' in kwargs:
            if kwargs['tile_type'] not in desc:
                continue
        
        # Check leader if specified
        if 'leader' in kwargs:
            if kwargs['leader'] not in desc:
                continue
        
        return idx
    
    return None

def display_board(game):
    """Display the current board state"""
    print("\n" + "=" * 50)
    print("Current Board State:")
    print("=" * 50)
    
    # Header row
    print("    ", end="")
    for col in range(16):
        print(chr(ord('A') + col), end=" ")
    print()
    
    board_str = game.get_board_state()
    lines = board_str.split('\n')
    
    # Find board lines (between BOARD: and P1:)
    in_board = False
    row = 1
    for line in lines:
        if line == 'BOARD:':
            in_board = True
            continue
        if line.startswith('P1:'):
            in_board = False
        if in_board and line:
            print(f"{row:2}: ", end="")
            for c in line:
                if c == '.':
                    print('. ', end="")
                elif c == '~':
                    print('~ ', end="")
                elif c == 'r':
                    print('\033[91mr\033[0m ', end="")  # Red
                elif c == 'g':
                    print('\033[92mg\033[0m ', end="")  # Green
                elif c == 'b':
                    print('\033[94mb\033[0m ', end="")  # Blue
                elif c == 'k':
                    print('\033[90mk\033[0m ', end="")  # Black
                elif c in '1234':  # P1 leaders
                    print(f'\033[93m{c}\033[0m ', end="")  # Yellow
                elif c in '5678':  # P2 leaders
                    print(f'\033[95m{c}\033[0m ', end="")  # Magenta
                else:
                    print(f'{c} ', end="")
            print()
            row += 1
    
    # Print rest of state
    for line in lines:
        if line.startswith('P1:') or line.startswith('P2:') or line.startswith('TURN:'):
            print(line)

def show_ai_suggestion(game, depth=5):
    """Show AI's suggested move"""
    ai_move = game.get_ai_move(depth)
    if ai_move is None:
        print("\nAI has no suggestion (game over?)")
        return None
    
    moves = game.get_legal_moves()
    for idx, desc in moves:
        if idx == ai_move:
            print(f"\n\033[92mAI SUGGESTS:\033[0m {desc}")
            return ai_move, desc
    
    return ai_move, "Unknown"

def print_help():
    """Print available commands"""
    print("""
Commands:
  tile <pos> <type>    - Place a tile (e.g., 'tile A3 red' or 't A3 r')
  leader <pos> <type>  - Place a leader (e.g., 'leader B2 black' or 'l B2 k')
  withdraw <pos>       - Withdraw a leader (e.g., 'withdraw C4' or 'w C4')
  catastrophe <pos>    - Place catastrophe (e.g., 'cat D5')
  pass                 - Pass action
  support <n>          - Add n support tiles in conflict
  treasure <pos>       - Take treasure
  monument <type>      - Build monument (e.g., 'monument red-black')
  decline              - Decline to build monument
  war <leader>         - Select leader to attack in war
  
  ai                   - Execute AI's suggested move
  moves                - Show all legal moves
  board                - Redisplay board
  undo                 - (not implemented)
  help                 - Show this help
  quit                 - Exit
  
Position format: Letter-Number (e.g., A1, P11)
Tile types: r/red, g/green, b/blue, k/black
Leader types: r/red (priest), g/green (trader), b/blue (farmer), k/black (king)
""")

def main():
    print("=" * 60)
    print("Tigris & Euphrates - BGA Assistant")
    print("=" * 60)
    print("This assistant helps you play T&E on BGA by suggesting moves.")
    print("Enter moves as they happen (yours and opponent's).")
    print("Type 'help' for commands.\n")
    
    game = tne.TnEGame()
    depth = 2
    
    while not game.is_game_over():
        display_board(game)
        
        moves = game.get_legal_moves()
        print(f"\nPlayer {game.current_player()}'s turn ({len(moves)} legal moves)")
        
        try:
            user_input = input("\n> ").strip().lower()
        except (EOFError, KeyboardInterrupt):
            print("\nExiting...")
            break
        
        if not user_input:
            continue
        
        parts = user_input.split()
        cmd = parts[0]
        
        move_idx = None
        
        if cmd in ['q', 'quit', 'exit']:
            break
            
        elif cmd in ['h', 'help', '?']:
            print_help()
            continue
            
        elif cmd in ['b', 'board']:
            continue  # Will redisplay
            
        elif cmd == 'moves':
            print("\nAll legal moves:")
            for idx, desc in moves:
                print(f"  [{idx}] {desc}")
            continue
            
        elif cmd == 'ai':
            suggestion = show_ai_suggestion(game, depth)
            print(suggestion)
            # if suggestion:
            #     move_idx = suggestion[0]
            # else:
            #     print("No AI suggestion available")
            #     continue
                
        elif cmd in ['t', 'tile']:
            if len(parts) < 3:
                print("Usage: tile <pos> <type>")
                continue
            pos = parse_position(parts[1])
            tile_type = parse_tile_type(parts[2])
            if pos is None or tile_type is None:
                print("Invalid position or tile type")
                continue
            move_idx = find_matching_move(game, 'PlaceTile', pos=pos, tile_type=tile_type)
            
        elif cmd in ['l', 'leader']:
            if len(parts) < 3:
                print("Usage: leader <pos> <type>")
                continue
            pos = parse_position(parts[1])
            leader = parse_leader(parts[2])
            if pos is None or leader is None:
                print("Invalid position or leader type")
                continue
            move_idx = find_matching_move(game, 'PlaceLeader', pos=pos, leader=leader)
            
        elif cmd in ['w', 'withdraw']:
            if len(parts) < 2:
                print("Usage: withdraw <pos>")
                continue
            pos = parse_position(parts[1])
            if pos is None:
                print("Invalid position")
                continue
            move_idx = find_matching_move(game, 'WithdrawLeader', pos=pos)
            
        elif cmd in ['c', 'cat', 'catastrophe']:
            if len(parts) < 2:
                print("Usage: catastrophe <pos>")
                continue
            pos = parse_position(parts[1])
            if pos is None:
                print("Invalid position")
                continue
            move_idx = find_matching_move(game, 'PlaceCatastrophe', pos=pos)
            
        elif cmd in ['p', 'pass']:
            move_idx = find_matching_move(game, 'Pass')
            
        elif cmd in ['s', 'support']:
            if len(parts) < 2:
                print("Usage: support <n>")
                continue
            n = int(parts[1])
            move_idx = find_matching_move(game, f'AddSupport({n})')
            
        elif cmd == 'treasure':
            if len(parts) < 2:
                print("Usage: treasure <pos>")
                continue
            pos = parse_position(parts[1])
            if pos is None:
                print("Invalid position")
                continue
            move_idx = find_matching_move(game, 'TakeTreasure', pos=pos)
            
        elif cmd == 'monument':
            if len(parts) < 2:
                print("Usage: monument <type1-type2>")
                continue
            move_idx = find_matching_move(game, 'BuildMonument')
            
        elif cmd == 'decline':
            move_idx = find_matching_move(game, 'DeclineMonument')
            
        elif cmd == 'war':
            if len(parts) < 2:
                print("Usage: war <leader>")
                continue
            leader = parse_leader(parts[1])
            if leader is None:
                print("Invalid leader type")
                continue
            move_idx = find_matching_move(game, 'WarSelectLeader', leader=leader)
            
        elif cmd == 'depth':
            if len(parts) >= 2:
                depth = int(parts[1])
                print(f"AI depth set to {depth}")
            continue
            
        else:
            # Try parsing as move index
            try:
                move_idx = int(cmd)
            except ValueError:
                print(f"Unknown command: {cmd}. Type 'help' for commands.")
                continue
        
        if move_idx is None:
            print("Could not find matching move. Use 'moves' to see all legal moves.")
            continue
        
        # Execute move
        result = game.process(move_idx)
        success, reward, flip = result
        
        if success:
            desc = next((d for i, d in moves if i == move_idx), "")
            print(f"\033[92m✓\033[0m Executed: {desc}")
            if flip:
                print("  (Turn changed)")
        else:
            print(f"\033[91m✗\033[0m Invalid move!")
    
    print("\n" + "=" * 60)
    print("GAME OVER")
    display_board(game)


if __name__ == "__main__":
    main()
