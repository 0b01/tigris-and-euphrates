#!/usr/bin/env python3
"""
BGA Bot for Tigris & Euphrates
Automates gameplay on Board Game Arena using Selenium
"""

import time
import json
import re
from dataclasses import dataclass
from typing import Optional, List, Tuple
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from webdriver_manager.chrome import ChromeDriverManager

import tne

# Board dimensions (T&E uses 16x11)
W, H = 16, 11

# Tile type mapping
TILE_COLORS = {
    'red': 'Red',      # Temple
    'green': 'Green',  # Market
    'blue': 'Blue',    # Farm
    'black': 'Black',  # Settlement
}

LEADER_COLORS = {
    'red': 'Red',      # Priest
    'green': 'Green',  # Trader
    'blue': 'Blue',    # Farmer
    'black': 'Black',  # King
}


@dataclass
class BGAGameState:
    """Parsed game state from BGA"""
    board: List[List[str]]  # 11x16 grid of tile types
    leaders: List[Tuple[int, int, str, int]]  # (x, y, color, player)
    treasures: List[Tuple[int, int]]
    catastrophes: List[Tuple[int, int]]
    monuments: List[Tuple[int, int, str, str]]  # (x, y, color1, color2)
    current_player: int  # 1 or 2
    my_player: int  # Which player am I
    scores: dict  # Player scores
    hand: dict  # My hand tiles
    game_phase: str  # "normal", "conflict", etc.


class TnEBGABot:
    """Bot that plays T&E on Board Game Arena"""
    
    def __init__(self, headless: bool = False):
        self.driver = None
        self.headless = headless
        self.game = None
        self.my_player = None
        self.ai_depth = 5
        
    def start_browser(self):
        """Initialize Chrome browser"""
        options = Options()
        if self.headless:
            options.add_argument("--headless")
        options.add_argument("--window-size=1920,1080")
        options.add_argument("--disable-gpu")
        options.add_argument("--no-sandbox")
        
        service = Service(ChromeDriverManager().install())
        self.driver = webdriver.Chrome(service=service, options=options)
        self.driver.implicitly_wait(10)
        print("Browser started")
        
    def login(self, username: str, password: str):
        """Login to BGA"""
        self.driver.get("https://boardgamearena.com/")
        time.sleep(2)
        
        # Click login button
        try:
            login_btn = self.driver.find_element(By.CSS_SELECTOR, "a[href*='login']")
            login_btn.click()
            time.sleep(1)
        except NoSuchElementException:
            pass  # Maybe already on login page
        
        # Enter credentials
        try:
            username_field = WebDriverWait(self.driver, 10).until(
                EC.presence_of_element_located((By.NAME, "email"))
            )
            username_field.send_keys(username)
            
            password_field = self.driver.find_element(By.NAME, "password")
            password_field.send_keys(password)
            
            submit_btn = self.driver.find_element(By.CSS_SELECTOR, "button[type='submit']")
            submit_btn.click()
            
            time.sleep(3)
            print("Logged in successfully")
        except Exception as e:
            print(f"Login failed: {e}")
            
    def go_to_game(self, game_url: str):
        """Navigate to a specific game"""
        self.driver.get(game_url)
        time.sleep(3)
        print(f"Navigated to game: {game_url}")
        
    def wait_for_my_turn(self, timeout: int = 300) -> bool:
        """Wait until it's my turn"""
        print("Waiting for my turn...")
        start_time = time.time()
        
        while time.time() - start_time < timeout:
            if self._is_my_turn():
                print("It's my turn!")
                return True
            time.sleep(2)
            
        print("Timeout waiting for turn")
        return False
    
    def _is_my_turn(self) -> bool:
        """Check if it's currently my turn"""
        try:
            # BGA typically shows active player with specific styling
            # Look for "Your turn" indicator or active player class
            turn_indicator = self.driver.find_elements(By.CSS_SELECTOR, ".your_turn, .active_player")
            if turn_indicator:
                return True
                
            # Alternative: check the game log or status area
            status = self.driver.find_element(By.ID, "pagemaintitletext")
            return "your turn" in status.text.lower() or "must" in status.text.lower()
        except:
            return False
    
    def parse_board_state(self) -> Optional[BGAGameState]:
        """Parse the current board state from BGA DOM"""
        try:
            state = BGAGameState(
                board=[['.' for _ in range(W)] for _ in range(H)],
                leaders=[],
                treasures=[],
                catastrophes=[],
                monuments=[],
                current_player=1,
                my_player=1,
                scores={1: {}, 2: {}},
                hand={},
                game_phase="normal"
            )
            
            # Parse tiles from the board
            # BGA T&E uses divs with data attributes for tiles
            tiles = self.driver.find_elements(By.CSS_SELECTOR, ".tile, [class*='tile']")
            for tile in tiles:
                try:
                    # Extract position and type from class/data attributes
                    classes = tile.get_attribute("class")
                    style = tile.get_attribute("style")
                    
                    # Parse position from transform or left/top
                    x, y = self._parse_position(tile)
                    if x is not None and y is not None:
                        tile_type = self._parse_tile_type(classes)
                        if tile_type and 0 <= x < W and 0 <= y < H:
                            state.board[y][x] = tile_type
                except Exception as e:
                    continue
            
            # Parse leaders
            leaders = self.driver.find_elements(By.CSS_SELECTOR, ".leader, [class*='leader']")
            for leader in leaders:
                try:
                    x, y = self._parse_position(leader)
                    color = self._parse_leader_color(leader.get_attribute("class"))
                    player = self._parse_leader_player(leader.get_attribute("class"))
                    if x is not None and color:
                        state.leaders.append((x, y, color, player))
                except:
                    continue
            
            # Determine which player I am
            state.my_player = self._determine_my_player()
            state.current_player = self._determine_current_player()
            
            return state
            
        except Exception as e:
            print(f"Error parsing board state: {e}")
            return None
    
    def _parse_position(self, element) -> Tuple[Optional[int], Optional[int]]:
        """Extract x, y position from element"""
        try:
            # Try data attributes first
            x = element.get_attribute("data-x")
            y = element.get_attribute("data-y")
            if x is not None and y is not None:
                return int(x), int(y)
            
            # Try parsing from ID (e.g., "tile_3_5")
            elem_id = element.get_attribute("id") or ""
            match = re.search(r'(\d+)[_-](\d+)', elem_id)
            if match:
                return int(match.group(1)), int(match.group(2))
            
            # Try parsing from style transform
            style = element.get_attribute("style") or ""
            match = re.search(r'left:\s*(\d+)px.*top:\s*(\d+)px', style)
            if match:
                # Convert pixel position to grid position (assume ~50px per cell)
                return int(match.group(1)) // 50, int(match.group(2)) // 50
                
        except:
            pass
        return None, None
    
    def _parse_tile_type(self, classes: str) -> Optional[str]:
        """Determine tile type from CSS classes"""
        if not classes:
            return None
        classes = classes.lower()
        if 'red' in classes or 'temple' in classes:
            return 'r'
        elif 'green' in classes or 'market' in classes:
            return 'g'
        elif 'blue' in classes or 'farm' in classes:
            return 'b'
        elif 'black' in classes or 'settlement' in classes:
            return 'k'
        elif 'catastrophe' in classes:
            return 'X'
        return None
    
    def _parse_leader_color(self, classes: str) -> Optional[str]:
        """Determine leader color from CSS classes"""
        if not classes:
            return None
        classes = classes.lower()
        for color in ['red', 'green', 'blue', 'black']:
            if color in classes:
                return color
        return None
    
    def _parse_leader_player(self, classes: str) -> int:
        """Determine which player owns leader from CSS classes"""
        if not classes:
            return 1
        # BGA typically uses player_1, player_2 or similar
        if 'player_2' in classes or 'opponent' in classes:
            return 2
        return 1
    
    def _determine_my_player(self) -> int:
        """Determine which player number I am"""
        try:
            # Look for "You" indicator or player panel positioning
            panels = self.driver.find_elements(By.CSS_SELECTOR, ".player-board, .player_panel")
            for i, panel in enumerate(panels):
                if 'current' in panel.get_attribute("class") or 'me' in panel.get_attribute("class"):
                    return i + 1
        except:
            pass
        return 1  # Default assumption
    
    def _determine_current_player(self) -> int:
        """Determine whose turn it is"""
        try:
            active = self.driver.find_elements(By.CSS_SELECTOR, ".active, .active_player")
            if active:
                classes = active[0].get_attribute("class")
                if 'player_2' in classes:
                    return 2
        except:
            pass
        return 1
    
    def sync_game_state(self, bga_state: BGAGameState):
        """Sync internal game with BGA state"""
        # For now, we'll use a simplified approach:
        # Start fresh game and try to match the state
        # In a full implementation, we'd track moves incrementally
        self.game = tne.TnEGame()
        print("Game state synced (fresh start)")
        
    def get_ai_move(self) -> Optional[int]:
        """Get the AI's recommended move"""
        if self.game is None:
            return None
        return self.game.get_ai_move(self.ai_depth)
    
    def execute_move(self, move_idx: int) -> bool:
        """Execute a move on BGA"""
        # Get legal moves to understand what this move represents
        legal_moves = self.game.get_legal_moves()
        
        for idx, desc in legal_moves:
            if idx == move_idx:
                print(f"Executing move: {desc}")
                return self._execute_action(desc)
        
        print(f"Move {move_idx} not found in legal moves")
        return False
    
    def _execute_action(self, action_desc: str) -> bool:
        """Execute an action on BGA based on description"""
        try:
            # Parse action type and parameters
            if "PlaceTile" in action_desc:
                return self._place_tile(action_desc)
            elif "PlaceLeader" in action_desc:
                return self._place_leader(action_desc)
            elif "WithdrawLeader" in action_desc:
                return self._withdraw_leader(action_desc)
            elif "PlaceCatastrophe" in action_desc:
                return self._place_catastrophe(action_desc)
            elif "Pass" in action_desc:
                return self._pass_action()
            elif "AddSupport" in action_desc:
                return self._add_support(action_desc)
            elif "TakeTreasure" in action_desc:
                return self._take_treasure(action_desc)
            elif "BuildMonument" in action_desc:
                return self._build_monument(action_desc)
            elif "DeclineMonument" in action_desc:
                return self._decline_monument()
            elif "WarSelectLeader" in action_desc:
                return self._select_war_leader(action_desc)
            else:
                print(f"Unknown action type: {action_desc}")
                return False
        except Exception as e:
            print(f"Error executing action: {e}")
            return False
    
    def _parse_pos_from_desc(self, desc: str) -> Optional[Tuple[int, int]]:
        """Extract position from action description"""
        # Format: "PlaceTile { pos: Pos { x: 3, y: 5 }, ... }"
        match = re.search(r'x:\s*(\d+).*y:\s*(\d+)', desc)
        if match:
            return int(match.group(1)), int(match.group(2))
        return None
    
    def _click_board_position(self, x: int, y: int) -> bool:
        """Click a position on the board"""
        try:
            # Find board element and calculate click position
            board = self.driver.find_element(By.ID, "board")
            
            # Click at the cell position (adjust based on actual BGA layout)
            cell_width = 50  # Approximate cell size
            cell_height = 50
            
            from selenium.webdriver.common.action_chains import ActionChains
            actions = ActionChains(self.driver)
            actions.move_to_element_with_offset(board, x * cell_width + 25, y * cell_height + 25)
            actions.click()
            actions.perform()
            
            time.sleep(0.5)
            return True
        except Exception as e:
            print(f"Error clicking position ({x}, {y}): {e}")
            return False
    
    def _place_tile(self, desc: str) -> bool:
        """Place a tile on the board"""
        pos = self._parse_pos_from_desc(desc)
        if not pos:
            return False
        
        # First select tile from hand
        tile_type = None
        for color in ['Red', 'Green', 'Blue', 'Black']:
            if color in desc:
                tile_type = color.lower()
                break
        
        if tile_type:
            # Click hand tile first
            try:
                hand_tile = self.driver.find_element(By.CSS_SELECTOR, f".hand .tile.{tile_type}")
                hand_tile.click()
                time.sleep(0.3)
            except:
                pass
        
        # Then click board position
        return self._click_board_position(pos[0], pos[1])
    
    def _place_leader(self, desc: str) -> bool:
        """Place a leader on the board"""
        pos = self._parse_pos_from_desc(desc)
        if not pos:
            return False
        
        # Select leader from panel
        leader_color = None
        for color in ['Red', 'Green', 'Blue', 'Black']:
            if color in desc:
                leader_color = color.lower()
                break
        
        if leader_color:
            try:
                leader = self.driver.find_element(By.CSS_SELECTOR, f".my-leaders .leader.{leader_color}")
                leader.click()
                time.sleep(0.3)
            except:
                pass
        
        return self._click_board_position(pos[0], pos[1])
    
    def _withdraw_leader(self, desc: str) -> bool:
        """Withdraw a leader from the board"""
        pos = self._parse_pos_from_desc(desc)
        if pos:
            return self._click_board_position(pos[0], pos[1])
        return False
    
    def _place_catastrophe(self, desc: str) -> bool:
        """Place a catastrophe tile"""
        pos = self._parse_pos_from_desc(desc)
        if not pos:
            return False
        
        # Select catastrophe from hand
        try:
            cat = self.driver.find_element(By.CSS_SELECTOR, ".catastrophe")
            cat.click()
            time.sleep(0.3)
        except:
            pass
        
        return self._click_board_position(pos[0], pos[1])
    
    def _pass_action(self) -> bool:
        """Pass the current action"""
        try:
            pass_btn = self.driver.find_element(By.CSS_SELECTOR, "button[id*='pass'], .pass-button")
            pass_btn.click()
            return True
        except:
            print("Could not find pass button")
            return False
    
    def _add_support(self, desc: str) -> bool:
        """Add support tiles during conflict"""
        match = re.search(r'AddSupport\((\d+)\)', desc)
        if match:
            count = int(match.group(1))
            try:
                # Click support button the appropriate number of times
                for _ in range(count):
                    support_btn = self.driver.find_element(By.CSS_SELECTOR, ".add-support, button[id*='support']")
                    support_btn.click()
                    time.sleep(0.2)
                
                # Confirm
                confirm_btn = self.driver.find_element(By.CSS_SELECTOR, ".confirm, button[id*='confirm']")
                confirm_btn.click()
                return True
            except:
                pass
        return False
    
    def _take_treasure(self, desc: str) -> bool:
        """Take a treasure from the board"""
        pos = self._parse_pos_from_desc(desc)
        if pos:
            return self._click_board_position(pos[0], pos[1])
        return False
    
    def _build_monument(self, desc: str) -> bool:
        """Build a monument"""
        try:
            # Click the monument option
            monument_btn = self.driver.find_element(By.CSS_SELECTOR, ".monument-option, button[id*='monument']")
            monument_btn.click()
            return True
        except:
            return False
    
    def _decline_monument(self) -> bool:
        """Decline to build a monument"""
        try:
            decline_btn = self.driver.find_element(By.CSS_SELECTOR, ".decline, button[id*='decline']")
            decline_btn.click()
            return True
        except:
            return False
    
    def _select_war_leader(self, desc: str) -> bool:
        """Select which leader to attack in war"""
        leader_color = None
        for color in ['Red', 'Green', 'Blue', 'Black']:
            if color in desc:
                leader_color = color.lower()
                break
        
        if leader_color:
            try:
                leader = self.driver.find_element(By.CSS_SELECTOR, f".war-target .leader.{leader_color}")
                leader.click()
                return True
            except:
                pass
        return False
    
    def play_turn(self) -> bool:
        """Play a single turn"""
        # Parse current state
        bga_state = self.parse_board_state()
        if not bga_state:
            print("Could not parse board state")
            return False
        
        # Sync game state
        self.sync_game_state(bga_state)
        
        # Get AI move
        move = self.get_ai_move()
        if move is None:
            print("AI returned no move")
            return False
        
        # Execute move
        return self.execute_move(move)
    
    def run(self, game_url: str, username: str = None, password: str = None):
        """Main bot loop"""
        self.start_browser()
        
        if username and password:
            self.login(username, password)
        
        self.go_to_game(game_url)
        
        print("Starting game loop...")
        while True:
            try:
                if self.wait_for_my_turn(timeout=60):
                    success = self.play_turn()
                    if not success:
                        print("Failed to play turn, waiting...")
                        time.sleep(5)
                else:
                    # Check if game ended
                    try:
                        game_over = self.driver.find_elements(By.CSS_SELECTOR, ".game_over, .endgame")
                        if game_over:
                            print("Game over!")
                            break
                    except:
                        pass
                    
            except KeyboardInterrupt:
                print("Stopping bot...")
                break
            except Exception as e:
                print(f"Error in game loop: {e}")
                time.sleep(5)
        
        self.driver.quit()


def main():
    import argparse
    parser = argparse.ArgumentParser(description="T&E BGA Bot")
    parser.add_argument("--game-url", required=True, help="BGA game URL")
    parser.add_argument("--username", help="BGA username")
    parser.add_argument("--password", help="BGA password")
    parser.add_argument("--depth", type=int, default=5, help="AI search depth")
    parser.add_argument("--headless", action="store_true", help="Run headless")
    args = parser.parse_args()
    
    bot = TnEBGABot(headless=args.headless)
    bot.ai_depth = args.depth
    bot.run(args.game_url, args.username, args.password)


if __name__ == "__main__":
    main()
