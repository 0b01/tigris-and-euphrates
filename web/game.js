// Tigris & Euphrates Web Game Logic
import init, { TnEWeb } from './pkg/tne.js';

// Sprite sheet configuration
// tiles.png: 18 sprites, each 12x13 pixels
// Index: 0=unification, 1=treasure_blue, 2=bull_blue, 3=bull_green, 4=bull_red, 5=bull_black
//        6=vase_blue, 7=vase_green, 8=vase_red, 9=vase_black
//        10=t_red, 11=t_red_treasure, 12=t_blue, 13=t_black, 14=t_green
//        15=circle, 16=warning, 17=catastrophe

const TILE_SPRITES = {
    unification: 0,
    treasure_blue: 1,
    bull_blue: 2,
    bull_green: 3,
    bull_red: 4,
    bull_black: 5,
    vase_blue: 6,
    vase_green: 7,
    vase_red: 8,
    vase_black: 9,
    t_red: 10,
    t_red_treasure: 11,
    t_blue: 12,
    t_black: 13,
    t_green: 14,
    circle: 15,
    warning: 16,
    catastrophe: 17
};

// monuments.png: 6 sprites, each 25x24 pixels
// Index: 0=red_blue, 1=black_green, 2=black_blue, 3=black_red, 4=green_blue, 5=red_green
const MONUMENT_SPRITES = {
    red_blue: 0,
    black_green: 1,
    black_blue: 2,
    black_red: 3,
    green_blue: 4,
    red_green: 5
};

const SPRITE_WIDTH = 12;
const SPRITE_HEIGHT = 13;
const MONUMENT_WIDTH = 25;
const MONUMENT_HEIGHT = 24;
const SCALE = 3;

// Map layout constants (matching the native visualizer)
const LOGICAL_W = 240;
const LOGICAL_H = 160;
const GRID_START_X = 17; // logical X offset for grid
const GRID_START_Y = 8;  // logical Y offset for grid
const CELL_SIZE = 13;    // 12px tile + 1px gap

// Hand position (right side of screen)
const HAND_X = 226;
const HAND_Y = 10;

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

class SpriteSheet {
    constructor(imageSrc, spriteWidth, spriteHeight) {
        this.image = new Image();
        this.image.src = imageSrc;
        this.spriteWidth = spriteWidth;
        this.spriteHeight = spriteHeight;
        this.loaded = false;
        this.image.onload = () => { this.loaded = true; };
    }

    draw(ctx, spriteIndex, x, y, scale = SCALE) {
        if (!this.loaded) return;
        const sx = spriteIndex * this.spriteWidth;
        ctx.imageSmoothingEnabled = false;
        ctx.drawImage(
            this.image,
            sx, 0, this.spriteWidth, this.spriteHeight,
            x, y, this.spriteWidth * scale, this.spriteHeight * scale
        );
    }
}

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
        
        // Load sprite sheets
        this.tileSprites = new SpriteSheet('tiles.png', SPRITE_WIDTH, SPRITE_HEIGHT);
        this.monumentSprites = new SpriteSheet('monuments.png', MONUMENT_WIDTH, MONUMENT_HEIGHT);
        this.mapImage = new Image();
        this.mapImage.src = 'map.png';
        
        this.canvas = null;
        this.ctx = null;
        
        this.init();
    }
    
    async init() {
        try {
            // Initialize WASM
            await init();
            this.game = new TnEWeb();
            
            this.setupCanvas();
            this.setupEventListeners();
            this.updateUI();
            
            // Start render loop
            this.render();
            
            console.log('Tigris & Euphrates initialized!');
        } catch (e) {
            console.error('Failed to initialize game:', e);
            document.getElementById('loading').innerHTML = 
                '<p style="color: red;">Failed to load game. Please refresh the page.</p>';
        }
    }
    
    setupCanvas() {
        const boardContainer = document.getElementById('board-container');
        this.canvas = document.createElement('canvas');
        // Use the full logical dimensions scaled up
        this.canvas.width = LOGICAL_W * SCALE;
        this.canvas.height = LOGICAL_H * SCALE;
        this.canvas.id = 'game-canvas';
        this.canvas.style.imageRendering = 'pixelated';
        this.canvas.style.cursor = 'pointer';
        
        // Replace the old board div
        const oldBoard = document.getElementById('board');
        if (oldBoard) oldBoard.remove();
        
        boardContainer.insertBefore(this.canvas, boardContainer.firstChild);
        this.ctx = this.canvas.getContext('2d');
        
        // Handle clicks on canvas
        this.canvas.addEventListener('click', (e) => this.onCanvasClick(e));
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
        } else {
            document.getElementById('btn-ai-vs-ai').classList.add('active');
            document.getElementById('ai-vs-ai-controls').classList.remove('hidden');
        }
        
        this.stopAutoPlay();
        this.newGame();
    }
    
    render() {
        if (!this.ctx || !this.gameState) {
            requestAnimationFrame(() => this.render());
            return;
        }
        
        // Clear canvas
        this.ctx.fillStyle = '#1a1a2e';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        
        // Draw board background (map) - scale it to fit
        if (this.mapImage.complete) {
            this.ctx.imageSmoothingEnabled = false;
            this.ctx.drawImage(this.mapImage, 0, 0, this.canvas.width, this.canvas.height);
        }
        
        // Helper function to convert grid position to pixel position
        const gridToPixel = (row, col) => ({
            x: (GRID_START_X + col * CELL_SIZE) * SCALE,
            y: (GRID_START_Y + row * CELL_SIZE) * SCALE
        });
        
        // Draw cells
        for (let x = 0; x < 11; x++) {
            for (let y = 0; y < 16; y++) {
                const pos = gridToPixel(x, y);
                const tileData = this.gameState.board.tiles[x][y];
                
                // Draw catastrophe
                if (tileData.is_catastrophe) {
                    this.tileSprites.draw(this.ctx, TILE_SPRITES.catastrophe, pos.x, pos.y);
                    continue;
                }
                
                // Skip monument cells (we'll draw them separately)
                if (tileData.is_monument) continue;
                
                // Draw tile
                if (tileData.tile_type !== 'empty') {
                    const hasTreasure = tileData.has_treasure;
                    let spriteIdx;
                    switch (tileData.tile_type) {
                        case 'red':
                            spriteIdx = hasTreasure ? TILE_SPRITES.t_red_treasure : TILE_SPRITES.t_red;
                            break;
                        case 'blue':
                            spriteIdx = TILE_SPRITES.t_blue;
                            break;
                        case 'green':
                            spriteIdx = TILE_SPRITES.t_green;
                            break;
                        case 'black':
                            spriteIdx = TILE_SPRITES.t_black;
                            break;
                    }
                    if (spriteIdx !== undefined) {
                        this.tileSprites.draw(this.ctx, spriteIdx, pos.x, pos.y);
                    }
                }
                
                // Draw leader
                if (tileData.leader) {
                    const isPlayer1 = tileData.leader_owner === 1;
                    let spriteIdx;
                    switch (tileData.leader) {
                        case 'red':
                            spriteIdx = isPlayer1 ? TILE_SPRITES.bull_red : TILE_SPRITES.vase_red;
                            break;
                        case 'blue':
                            spriteIdx = isPlayer1 ? TILE_SPRITES.bull_blue : TILE_SPRITES.vase_blue;
                            break;
                        case 'green':
                            spriteIdx = isPlayer1 ? TILE_SPRITES.bull_green : TILE_SPRITES.vase_green;
                            break;
                        case 'black':
                            spriteIdx = isPlayer1 ? TILE_SPRITES.bull_black : TILE_SPRITES.vase_black;
                            break;
                    }
                    if (spriteIdx !== undefined) {
                        this.tileSprites.draw(this.ctx, spriteIdx, pos.x, pos.y);
                    }
                    
                    // Draw conflict marker if in conflict
                    if (this.gameState.internal_conflict) {
                        const ic = this.gameState.internal_conflict;
                        if ((ic.attacker_pos.x === x && ic.attacker_pos.y === y) ||
                            (ic.defender_pos.x === x && ic.defender_pos.y === y)) {
                            this.tileSprites.draw(this.ctx, TILE_SPRITES.warning, pos.x, pos.y);
                        }
                    }
                }
                
                // Draw treasure marker for non-red tiles
                if (tileData.has_treasure && tileData.tile_type !== 'red') {
                    this.tileSprites.draw(this.ctx, TILE_SPRITES.treasure_blue, pos.x, pos.y);
                }
            }
        }
        
        // Draw monuments (2x2)
        this.gameState.monuments.forEach(monument => {
            const pos = gridToPixel(monument.pos_top_left.x, monument.pos_top_left.y);
            const spriteIdx = MONUMENT_SPRITES[monument.monument_type];
            if (spriteIdx !== undefined) {
                this.monumentSprites.draw(this.ctx, spriteIdx, pos.x, pos.y, SCALE);
            }
        });
        
        // Draw unification tile marker
        if (this.gameState.external_conflict) {
            const upos = this.gameState.external_conflict.unification_tile_pos;
            const pos = gridToPixel(upos.x, upos.y);
            this.tileSprites.draw(this.ctx, TILE_SPRITES.unification, pos.x, pos.y);
        }
        
        // Draw valid move highlights
        if (this.selectedItem && this.gameState.current_player === 1 && this.mode === 'human-vs-ai') {
            this.ctx.strokeStyle = '#f1c40f';
            this.ctx.lineWidth = 2;
            
            const movesJson = this.game.get_valid_moves();
            const moves = JSON.parse(movesJson);
            
            moves.forEach(move => {
                if (move.action.pos) {
                    let matches = false;
                    if (this.selectedItem.type === 'tile' && move.action.action_type === 'place_tile') {
                        matches = move.action.tile_type === this.selectedItem.color;
                    } else if (this.selectedItem.type === 'leader' && move.action.action_type === 'place_leader') {
                        matches = move.action.leader === this.selectedItem.color;
                    }
                    
                    if (matches) {
                        const pos = gridToPixel(move.action.pos.x, move.action.pos.y);
                        this.ctx.strokeRect(pos.x, pos.y, SPRITE_WIDTH * SCALE, SPRITE_HEIGHT * SCALE);
                    }
                }
            });
        }
        
        // Draw treasure selection highlights
        if (this.gameState.next_action.action_type === 'take_treasure') {
            this.ctx.strokeStyle = '#00ff00';
            this.ctx.lineWidth = 3;
            const treasures = this.gameState.next_action.options?.treasures || [];
            treasures.forEach(tpos => {
                const pos = gridToPixel(tpos.x, tpos.y);
                this.ctx.strokeRect(pos.x, pos.y, SPRITE_WIDTH * SCALE, SPRITE_HEIGHT * SCALE);
            });
        }
        
        // Draw player hand on canvas (right side, using sprites)
        if (this.mode === 'human-vs-ai' && this.gameState.current_player === 1) {
            this.renderHandOnCanvas();
        }
        
        requestAnimationFrame(() => this.render());
    }
    
    renderHandOnCanvas() {
        const player = this.gameState.player1;
        let handIndex = 0;
        
        // Draw tiles in hand
        const handTiles = [
            { color: 'red', count: player.hand_red, sprite: TILE_SPRITES.t_red },
            { color: 'blue', count: player.hand_blue, sprite: TILE_SPRITES.t_blue },
            { color: 'green', count: player.hand_green, sprite: TILE_SPRITES.t_green },
            { color: 'black', count: player.hand_black, sprite: TILE_SPRITES.t_black }
        ];
        
        handTiles.forEach(({ color, count, sprite }) => {
            for (let i = 0; i < count; i++) {
                const px = HAND_X * SCALE;
                const py = (HAND_Y + handIndex * 14) * SCALE;
                
                // Highlight if selected
                if (this.selectedItem?.type === 'tile' && this.selectedItem?.color === color) {
                    this.ctx.strokeStyle = '#f1c40f';
                    this.ctx.lineWidth = 2;
                    this.ctx.strokeRect(px - 2, py - 2, SPRITE_WIDTH * SCALE + 4, SPRITE_HEIGHT * SCALE + 4);
                }
                
                this.tileSprites.draw(this.ctx, sprite, px, py);
                
                // Store position for click detection
                if (!this.handPositions) this.handPositions = [];
                this.handPositions[handIndex] = { type: 'tile', color, x: px, y: py };
                
                handIndex++;
            }
        });
        
        // Draw available leaders
        ['red', 'blue', 'green', 'black'].forEach(color => {
            const onBoard = player.leaders_on_board.includes(color);
            if (!onBoard) {
                const px = HAND_X * SCALE;
                const py = (HAND_Y + handIndex * 14) * SCALE;
                
                const sprite = {
                    red: TILE_SPRITES.bull_red,
                    blue: TILE_SPRITES.bull_blue,
                    green: TILE_SPRITES.bull_green,
                    black: TILE_SPRITES.bull_black
                }[color];
                
                // Highlight if selected
                if (this.selectedItem?.type === 'leader' && this.selectedItem?.color === color) {
                    this.ctx.strokeStyle = '#f1c40f';
                    this.ctx.lineWidth = 2;
                    this.ctx.strokeRect(px - 2, py - 2, SPRITE_WIDTH * SCALE + 4, SPRITE_HEIGHT * SCALE + 4);
                }
                
                this.tileSprites.draw(this.ctx, sprite, px, py);
                
                // Store position for click detection
                if (!this.handPositions) this.handPositions = [];
                this.handPositions[handIndex] = { type: 'leader', color, x: px, y: py };
                
                handIndex++;
            }
        });
        
        // Trim any extra positions from previous renders
        if (this.handPositions) {
            this.handPositions.length = handIndex;
        }
    }
    
    onCanvasClick(e) {
        if (this.gameState.current_player !== 1 && this.mode === 'human-vs-ai') {
            return; // Not player's turn
        }
        
        const rect = this.canvas.getBoundingClientRect();
        const scaleX = this.canvas.width / rect.width;
        const scaleY = this.canvas.height / rect.height;
        const clickX = (e.clientX - rect.left) * scaleX;
        const clickY = (e.clientY - rect.top) * scaleY;
        
        // Check if clicked on hand items first
        if (this.handPositions) {
            for (const item of this.handPositions) {
                if (clickX >= item.x && clickX <= item.x + SPRITE_WIDTH * SCALE &&
                    clickY >= item.y && clickY <= item.y + SPRITE_HEIGHT * SCALE) {
                    this.selectItem(item.type, item.color);
                    return;
                }
            }
        }
        
        // Convert click to grid position
        const logicalX = clickX / SCALE;
        const logicalY = clickY / SCALE;
        
        // Calculate grid cell
        const col = Math.floor((logicalX - GRID_START_X) / CELL_SIZE);
        const row = Math.floor((logicalY - GRID_START_Y) / CELL_SIZE);
        
        if (row < 0 || row >= 11 || col < 0 || col >= 16) return;
        
        const x = row;
        const y = col;
        
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
    
    updateUI() {
        const stateJson = this.game.get_state();
        this.gameState = JSON.parse(stateJson);
        
        if (this.gameState.error) {
            console.error('Game state error:', this.gameState.error);
            return;
        }
        
        this.updatePlayerPanels();
        this.updateGameStatus();
        this.checkDialogs();
        this.checkGameOver();
        
        // Check if AI should play
        if (this.mode === 'human-vs-ai' && this.gameState.current_player === 2 && !this.gameState.is_game_over) {
            this.scheduleAIMove();
        }
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
        
        // Hand is now rendered on canvas, so we just reset hand positions
        this.handPositions = [];
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
