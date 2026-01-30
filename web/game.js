// Tigris & Euphrates Web Game Logic
import init, { TnEWeb } from './pkg/tne.js';

// River positions on the board (based on the original game)
const RIVER_POSITIONS = new Set([
    '0,5', '0,6', '0,7', '0,8', '0,9',
    '1,5', '1,9',
    '2,5', '2,9',
    '3,4', '3,5', '3,9', '3,10',
    '4,3', '4,4', '4,10', '4,11', '4,12',
    '5,2', '5,3', '5,12', '5,13',
    '6,1', '6,2', '6,13', '6,14',
    '7,0', '7,1', '7,14', '7,15',
    '8,0', '8,15',
    '9,0', '9,15',
    '10,0', '10,1', '10,2', '10,3', '10,4', '10,5', '10,6', '10,7', '10,8', '10,9', '10,10', '10,11', '10,12', '10,13', '10,14', '10,15'
]);

class TnEGameUI {
    constructor() {
        this.game = null;
        this.gameState = null;
        this.selectedItem = null; // { type: 'tile' | 'leader', color: string }
        this.validMoves = [];
        this.mode = 'human-vs-ai'; // 'human-vs-ai' or 'ai-vs-ai'
        this.aiAutoPlay = false;
        this.aiPlayInterval = null;
        this.aiSpeed = 500;
        
        this.init();
    }
    
    async init() {
        try {
            // Initialize WASM
            await init();
            this.game = new TnEWeb();
            
            this.setupEventListeners();
            this.renderBoard();
            this.updateUI();
            
            console.log('Tigris & Euphrates initialized!');
        } catch (e) {
            console.error('Failed to initialize game:', e);
            document.getElementById('loading').innerHTML = 
                '<p style="color: red;">Failed to load game. Please refresh the page.</p>';
        }
    }
    
    setupEventListeners() {
        // Mode buttons
        document.getElementById('btn-human-vs-ai').addEventListener('click', () => this.setMode('human-vs-ai'));
        document.getElementById('btn-ai-vs-ai').addEventListener('click', () => this.setMode('ai-vs-ai'));
        
        // Action buttons
        document.getElementById('btn-pass').addEventListener('click', () => this.pass());
        document.getElementById('btn-new-game').addEventListener('click', () => this.newGame());
        document.getElementById('btn-play-again').addEventListener('click', () => this.newGame());
        
        // AI depth selector
        document.getElementById('ai-depth').addEventListener('change', (e) => {
            this.game.set_ai_depth(parseInt(e.target.value));
        });
        
        // AI vs AI controls
        document.getElementById('btn-ai-step').addEventListener('click', () => this.aiStep());
        document.getElementById('btn-ai-play').addEventListener('click', () => this.startAutoPlay());
        document.getElementById('btn-ai-pause').addEventListener('click', () => this.stopAutoPlay());
        document.getElementById('ai-speed').addEventListener('input', (e) => {
            this.aiSpeed = parseInt(e.target.value);
            if (this.aiAutoPlay) {
                this.stopAutoPlay();
                this.startAutoPlay();
            }
        });
        
        // Monument dialog
        document.getElementById('btn-decline-monument').addEventListener('click', () => this.declineMonument());
    }
    
    setMode(mode) {
        this.mode = mode;
        document.querySelectorAll('.mode-btn').forEach(btn => btn.classList.remove('active'));
        
        if (mode === 'human-vs-ai') {
            document.getElementById('btn-human-vs-ai').classList.add('active');
            document.getElementById('ai-vs-ai-controls').classList.add('hidden');
            document.getElementById('p1-hand').classList.remove('hidden');
            document.getElementById('p1-leaders').classList.remove('hidden');
        } else {
            document.getElementById('btn-ai-vs-ai').classList.add('active');
            document.getElementById('ai-vs-ai-controls').classList.remove('hidden');
            document.getElementById('p1-hand').classList.add('hidden');
            document.getElementById('p1-leaders').classList.add('hidden');
        }
        
        this.stopAutoPlay();
        this.newGame();
    }
    
    renderBoard() {
        const board = document.getElementById('board');
        board.innerHTML = '';
        
        for (let x = 0; x < 11; x++) {
            for (let y = 0; y < 16; y++) {
                const cell = document.createElement('div');
                cell.className = 'cell';
                cell.dataset.x = x;
                cell.dataset.y = y;
                
                if (RIVER_POSITIONS.has(`${x},${y}`)) {
                    cell.classList.add('river');
                }
                
                cell.addEventListener('click', () => this.onCellClick(x, y));
                board.appendChild(cell);
            }
        }
    }
    
    updateUI() {
        const stateJson = this.game.get_state();
        this.gameState = JSON.parse(stateJson);
        
        if (this.gameState.error) {
            console.error('Game state error:', this.gameState.error);
            return;
        }
        
        this.updateBoard();
        this.updatePlayerPanels();
        this.updateGameStatus();
        this.updateValidMoves();
        this.checkDialogs();
        this.checkGameOver();
        
        // Check if AI should play
        if (this.mode === 'human-vs-ai' && this.gameState.current_player === 2 && !this.gameState.is_game_over) {
            this.scheduleAIMove();
        }
    }
    
    updateBoard() {
        const cells = document.querySelectorAll('.cell');
        
        cells.forEach(cell => {
            const x = parseInt(cell.dataset.x);
            const y = parseInt(cell.dataset.y);
            const tileData = this.gameState.board.tiles[x][y];
            
            // Reset cell classes
            cell.className = 'cell';
            if (RIVER_POSITIONS.has(`${x},${y}`)) {
                cell.classList.add('river');
            }
            
            // Clear cell content
            cell.innerHTML = '';
            
            if (tileData.is_catastrophe) {
                cell.classList.add('catastrophe');
                cell.innerHTML = '<span style="font-size: 1.2rem;">💥</span>';
            } else if (tileData.is_monument) {
                cell.classList.add('monument');
            } else if (tileData.leader) {
                const content = document.createElement('div');
                content.className = `cell-content leader tile-${tileData.leader} player${tileData.leader_owner}`;
                content.textContent = this.getLeaderSymbol(tileData.leader, tileData.leader_owner);
                cell.appendChild(content);
                
                // Check for conflict markers
                if (this.gameState.internal_conflict) {
                    const ic = this.gameState.internal_conflict;
                    if ((ic.attacker_pos.x === x && ic.attacker_pos.y === y) ||
                        (ic.defender_pos.x === x && ic.defender_pos.y === y)) {
                        const marker = document.createElement('div');
                        marker.className = 'conflict-marker';
                        cell.appendChild(marker);
                    }
                }
            } else if (tileData.tile_type !== 'empty') {
                const content = document.createElement('div');
                content.className = `cell-content tile-${tileData.tile_type}`;
                cell.appendChild(content);
            }
            
            // Add treasure marker
            if (tileData.has_treasure) {
                const treasure = document.createElement('span');
                treasure.className = 'treasure-marker';
                treasure.textContent = '💎';
                cell.appendChild(treasure);
            }
        });
        
        // Render monuments (2x2 overlays)
        this.renderMonuments();
    }
    
    renderMonuments() {
        // Remove existing monument overlays
        document.querySelectorAll('.monument-overlay').forEach(el => el.remove());
        
        const board = document.getElementById('board');
        
        this.gameState.monuments.forEach(monument => {
            const cell = document.querySelector(`.cell[data-x="${monument.pos_top_left.x}"][data-y="${monument.pos_top_left.y}"]`);
            if (!cell) return;
            
            const overlay = document.createElement('div');
            overlay.className = 'monument-overlay';
            
            const colors = monument.monument_type.split('_');
            const colorsDiv = document.createElement('div');
            colorsDiv.className = 'monument-colors';
            
            colors.forEach(color => {
                const colorDiv = document.createElement('div');
                colorDiv.className = `monument-color`;
                colorDiv.style.background = this.getColorValue(color);
                colorsDiv.appendChild(colorDiv);
            });
            
            overlay.appendChild(colorsDiv);
            
            // Position the overlay
            const rect = cell.getBoundingClientRect();
            const boardRect = board.getBoundingClientRect();
            overlay.style.left = `${cell.offsetLeft}px`;
            overlay.style.top = `${cell.offsetTop}px`;
            
            board.appendChild(overlay);
        });
    }
    
    getLeaderSymbol(color, player) {
        const symbols = {
            red: player === 1 ? '🔴' : '🔴',
            blue: player === 1 ? '🔵' : '🔵',
            green: player === 1 ? '🟢' : '🟢',
            black: player === 1 ? '⚫' : '⚫'
        };
        return player === 1 ? '●' : '○';
    }
    
    getColorValue(color) {
        const colors = {
            red: '#e74c3c',
            blue: '#3498db',
            green: '#27ae60',
            black: '#2c3e50'
        };
        return colors[color] || '#888';
    }
    
    updatePlayerPanels() {
        const p1 = this.gameState.player1;
        const p2 = this.gameState.player2;
        
        // Update scores
        document.getElementById('p1-score-red').textContent = p1.score_red;
        document.getElementById('p1-score-blue').textContent = p1.score_blue;
        document.getElementById('p1-score-green').textContent = p1.score_green;
        document.getElementById('p1-score-black').textContent = p1.score_black;
        document.getElementById('p1-score-treasure').textContent = p1.score_treasure;
        document.getElementById('p1-final-score').textContent = p1.final_score;
        document.getElementById('p1-catastrophes').textContent = p1.num_catastrophes;
        
        document.getElementById('p2-score-red').textContent = p2.score_red;
        document.getElementById('p2-score-blue').textContent = p2.score_blue;
        document.getElementById('p2-score-green').textContent = p2.score_green;
        document.getElementById('p2-score-black').textContent = p2.score_black;
        document.getElementById('p2-score-treasure').textContent = p2.score_treasure;
        document.getElementById('p2-final-score').textContent = p2.final_score;
        
        // Update active player highlighting
        document.getElementById('player1-panel').classList.toggle('active', this.gameState.current_player === 1);
        document.getElementById('player2-panel').classList.toggle('active', this.gameState.current_player === 2);
        
        // Update hand for Player 1
        if (this.mode === 'human-vs-ai') {
            this.renderHand(p1);
            this.renderLeaders(p1);
        }
    }
    
    renderHand(player) {
        const hand = document.getElementById('p1-hand');
        hand.innerHTML = '';
        
        const tiles = [
            { color: 'red', count: player.hand_red },
            { color: 'blue', count: player.hand_blue },
            { color: 'green', count: player.hand_green },
            { color: 'black', count: player.hand_black }
        ];
        
        tiles.forEach(({ color, count }) => {
            for (let i = 0; i < count; i++) {
                const tile = document.createElement('div');
                tile.className = `hand-tile tile ${color}`;
                tile.dataset.type = 'tile';
                tile.dataset.color = color;
                tile.addEventListener('click', () => this.selectItem('tile', color));
                
                if (this.selectedItem?.type === 'tile' && this.selectedItem?.color === color) {
                    tile.classList.add('selected');
                }
                
                hand.appendChild(tile);
            }
        });
    }
    
    renderLeaders(player) {
        const leaders = document.getElementById('p1-leaders');
        leaders.innerHTML = '';
        
        ['red', 'blue', 'green', 'black'].forEach(color => {
            const onBoard = player.leaders_on_board.includes(color);
            const leader = document.createElement('div');
            leader.className = `leader-piece`;
            leader.style.background = this.getColorValue(color);
            leader.dataset.type = 'leader';
            leader.dataset.color = color;
            
            if (onBoard) {
                leader.classList.add('on-board');
                leader.title = `${color} leader is on the board`;
            } else {
                leader.addEventListener('click', () => this.selectItem('leader', color));
                leader.title = `Place ${color} leader`;
            }
            
            if (this.selectedItem?.type === 'leader' && this.selectedItem?.color === color) {
                leader.classList.add('selected');
            }
            
            leaders.appendChild(leader);
        });
    }
    
    updateGameStatus() {
        const playerText = this.gameState.current_player === 1 ? "Player 1's turn" : "Player 2's turn (AI)";
        document.getElementById('current-player').textContent = playerText;
        
        const stateText = {
            'normal': 'Normal action',
            'take_treasure': 'Take a treasure',
            'add_support': 'Add support tiles',
            'war_select_leader': 'Select leader for war',
            'build_monument': 'Build monument?'
        }[this.gameState.game_state] || this.gameState.game_state;
        
        document.getElementById('game-state').textContent = stateText;
    }
    
    updateValidMoves() {
        // Clear previous valid move indicators
        document.querySelectorAll('.cell.valid-move').forEach(cell => cell.classList.remove('valid-move'));
        
        if (this.selectedItem && this.gameState.current_player === 1 && this.mode === 'human-vs-ai') {
            const movesJson = this.game.get_valid_moves();
            const moves = JSON.parse(movesJson);
            
            moves.forEach(move => {
                if (move.action.pos) {
                    const { x, y } = move.action.pos;
                    
                    // Check if move matches selected item
                    let matches = false;
                    if (this.selectedItem.type === 'tile' && move.action.action_type === 'place_tile') {
                        matches = move.action.tile_type === this.selectedItem.color;
                    } else if (this.selectedItem.type === 'leader' && move.action.action_type === 'place_leader') {
                        matches = move.action.leader === this.selectedItem.color;
                    }
                    
                    if (matches) {
                        const cell = document.querySelector(`.cell[data-x="${x}"][data-y="${y}"]`);
                        if (cell) cell.classList.add('valid-move');
                    }
                }
            });
        }
    }
    
    checkDialogs() {
        const action = this.gameState.next_action;
        
        // Hide all dialogs first
        document.getElementById('support-dialog').classList.add('hidden');
        document.getElementById('monument-dialog').classList.add('hidden');
        document.getElementById('war-leader-dialog').classList.add('hidden');
        
        if (this.gameState.current_player !== 1 && this.mode === 'human-vs-ai') {
            return; // Don't show dialogs during AI turn
        }
        
        if (action.action_type === 'add_support' && this.mode === 'human-vs-ai') {
            this.showSupportDialog(action.options?.tile_type);
        } else if (action.action_type === 'build_monument' && this.mode === 'human-vs-ai') {
            this.showMonumentDialog(action.options?.monuments);
        } else if (action.action_type === 'select_leader' && this.mode === 'human-vs-ai') {
            this.showWarLeaderDialog(action.options?.leaders);
        } else if (action.action_type === 'take_treasure') {
            // Highlight treasure positions
            const treasures = action.options?.treasures || [];
            treasures.forEach(pos => {
                const cell = document.querySelector(`.cell[data-x="${pos.x}"][data-y="${pos.y}"]`);
                if (cell) cell.classList.add('valid-move');
            });
        }
    }
    
    showSupportDialog(tileType) {
        const dialog = document.getElementById('support-dialog');
        const options = document.getElementById('support-options');
        options.innerHTML = '';
        
        const player = this.gameState.current_player === 1 ? this.gameState.player1 : this.gameState.player2;
        const maxSupport = {
            red: player.hand_red,
            blue: player.hand_blue,
            green: player.hand_green,
            black: player.hand_black
        }[tileType] || 0;
        
        for (let i = 0; i <= maxSupport; i++) {
            const btn = document.createElement('button');
            btn.className = 'support-btn';
            btn.textContent = i;
            btn.addEventListener('click', () => this.addSupport(i));
            options.appendChild(btn);
        }
        
        dialog.classList.remove('hidden');
    }
    
    showMonumentDialog(monumentTypes) {
        const dialog = document.getElementById('monument-dialog');
        const options = document.getElementById('monument-options');
        options.innerHTML = '';
        
        (monumentTypes || []).forEach(type => {
            const btn = document.createElement('button');
            btn.className = 'monument-btn';
            
            const colors = type.split('_');
            colors.forEach(color => {
                const colorDiv = document.createElement('div');
                colorDiv.className = 'monument-color';
                colorDiv.style.background = this.getColorValue(color);
                btn.appendChild(colorDiv);
            });
            
            btn.addEventListener('click', () => this.buildMonument(type));
            options.appendChild(btn);
        });
        
        dialog.classList.remove('hidden');
    }
    
    showWarLeaderDialog(leaders) {
        const dialog = document.getElementById('war-leader-dialog');
        const options = document.getElementById('war-leader-options');
        options.innerHTML = '';
        
        (leaders || []).forEach(leader => {
            const btn = document.createElement('button');
            btn.className = 'leader-btn';
            btn.style.background = this.getColorValue(leader);
            btn.textContent = leader.charAt(0).toUpperCase() + leader.slice(1);
            btn.addEventListener('click', () => this.selectWarLeader(leader));
            options.appendChild(btn);
        });
        
        dialog.classList.remove('hidden');
    }
    
    checkGameOver() {
        if (this.gameState.is_game_over) {
            this.stopAutoPlay();
            
            const dialog = document.getElementById('game-over-dialog');
            const winnerText = document.getElementById('winner-text');
            
            const winner = this.gameState.winner;
            if (winner === 1) {
                winnerText.textContent = 'Player 1 Wins!';
            } else if (winner === 2) {
                winnerText.textContent = 'Player 2 (AI) Wins!';
            } else {
                winnerText.textContent = "It's a Draw!";
            }
            
            document.getElementById('final-p1-score').textContent = this.gameState.player1.final_score;
            document.getElementById('final-p2-score').textContent = this.gameState.player2.final_score;
            
            dialog.classList.remove('hidden');
        }
    }
    
    selectItem(type, color) {
        if (this.selectedItem?.type === type && this.selectedItem?.color === color) {
            this.selectedItem = null;
        } else {
            this.selectedItem = { type, color };
        }
        this.updateUI();
    }
    
    onCellClick(x, y) {
        if (this.gameState.current_player !== 1 && this.mode === 'human-vs-ai') {
            return; // Not player's turn
        }
        
        const action = this.gameState.next_action;
        
        // Handle treasure taking
        if (action.action_type === 'take_treasure') {
            const treasures = action.options?.treasures || [];
            if (treasures.some(t => t.x === x && t.y === y)) {
                this.takeTreasure(x, y);
                return;
            }
        }
        
        // Handle tile/leader placement
        if (this.selectedItem && action.action_type === 'normal') {
            if (this.selectedItem.type === 'tile') {
                this.placeTile(x, y, this.selectedItem.color);
            } else if (this.selectedItem.type === 'leader') {
                this.placeLeader(x, y, this.selectedItem.color);
            }
        }
    }
    
    placeTile(x, y, tileType) {
        const action = {
            action_type: 'place_tile',
            pos: { x, y },
            tile_type: tileType
        };
        
        this.applyAction(action);
    }
    
    placeLeader(x, y, leader) {
        const action = {
            action_type: 'place_leader',
            pos: { x, y },
            leader: leader
        };
        
        this.applyAction(action);
    }
    
    takeTreasure(x, y) {
        const action = {
            action_type: 'take_treasure',
            pos: { x, y }
        };
        
        this.applyAction(action);
    }
    
    addSupport(n) {
        const action = {
            action_type: 'add_support',
            support: n
        };
        
        document.getElementById('support-dialog').classList.add('hidden');
        this.applyAction(action);
    }
    
    buildMonument(monumentType) {
        const action = {
            action_type: 'build_monument',
            monument_type: monumentType
        };
        
        document.getElementById('monument-dialog').classList.add('hidden');
        this.applyAction(action);
    }
    
    declineMonument() {
        const action = {
            action_type: 'decline_monument'
        };
        
        document.getElementById('monument-dialog').classList.add('hidden');
        this.applyAction(action);
    }
    
    selectWarLeader(leader) {
        const action = {
            action_type: 'war_select_leader',
            leader: leader
        };
        
        document.getElementById('war-leader-dialog').classList.add('hidden');
        this.applyAction(action);
    }
    
    pass() {
        if (this.gameState.current_player !== 1 && this.mode === 'human-vs-ai') {
            return;
        }
        
        const action = { action_type: 'pass' };
        this.applyAction(action);
    }
    
    applyAction(action) {
        const result = this.game.apply_action(JSON.stringify(action));
        const resultData = JSON.parse(result);
        
        if (resultData.error) {
            console.error('Action error:', resultData.error);
            return false;
        }
        
        this.selectedItem = null;
        this.updateUI();
        return true;
    }
    
    scheduleAIMove() {
        document.getElementById('loading').classList.remove('hidden');
        
        setTimeout(() => {
            this.game.ai_play();
            document.getElementById('loading').classList.add('hidden');
            this.updateUI();
        }, 100);
    }
    
    aiStep() {
        if (this.gameState.is_game_over) return;
        
        document.getElementById('loading').classList.remove('hidden');
        
        setTimeout(() => {
            this.game.ai_play();
            document.getElementById('loading').classList.add('hidden');
            this.updateUI();
        }, 50);
    }
    
    startAutoPlay() {
        if (this.aiAutoPlay) return;
        
        this.aiAutoPlay = true;
        document.getElementById('btn-ai-play').classList.add('hidden');
        document.getElementById('btn-ai-pause').classList.remove('hidden');
        
        this.aiPlayInterval = setInterval(() => {
            if (this.gameState.is_game_over) {
                this.stopAutoPlay();
                return;
            }
            this.game.ai_play();
            this.updateUI();
        }, this.aiSpeed);
    }
    
    stopAutoPlay() {
        this.aiAutoPlay = false;
        document.getElementById('btn-ai-play').classList.remove('hidden');
        document.getElementById('btn-ai-pause').classList.add('hidden');
        
        if (this.aiPlayInterval) {
            clearInterval(this.aiPlayInterval);
            this.aiPlayInterval = null;
        }
    }
    
    newGame() {
        this.stopAutoPlay();
        this.game.reset();
        this.selectedItem = null;
        document.getElementById('game-over-dialog').classList.add('hidden');
        this.updateUI();
    }
}

// Initialize the game when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new TnEGameUI();
});
