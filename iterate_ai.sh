#!/bin/bash
# AI Iteration Script
# Runs games, generates logs for analysis, then you improve the AI

set -e

LOGDIR="logs"
mkdir -p "$LOGDIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOGFILE="$LOGDIR/game_$TIMESTAMP.log"

echo "=== AI Iteration Run: $TIMESTAMP ===" | tee "$LOGFILE"
echo "" | tee -a "$LOGFILE"

# Build
echo "Building..." | tee -a "$LOGFILE"
cargo build --release --features game 2>&1 | tail -5

# Run selfplay and capture output
echo "" | tee -a "$LOGFILE"
echo "Running 50-move selfplay..." | tee -a "$LOGFILE"
echo "" | tee -a "$LOGFILE"

cargo run --release --features game -- selfplay 2>&1 | tee -a "$LOGFILE"

echo "" | tee -a "$LOGFILE"
echo "=== Game log saved to: $LOGFILE ===" 
echo ""
echo "To analyze: cat $LOGFILE"
echo "Latest log: ls -t $LOGDIR/*.log | head -1"
