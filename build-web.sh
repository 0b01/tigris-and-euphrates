#!/bin/bash

# Build script for Tigris & Euphrates Web Version
set -e

echo "🏗️  Building Tigris & Euphrates WASM..."

# Check if wasm-pack is installed
if ! command -v wasm-pack &> /dev/null; then
    echo "📦 Installing wasm-pack..."
    cargo install wasm-pack
fi

# Build the WASM module
echo "🔨 Compiling Rust to WebAssembly..."
wasm-pack build --target web --out-dir web/pkg --features wasm

# The output will be in web/pkg/
echo "✅ Build complete!"
echo ""
echo "📁 Output files in web/pkg/"
echo ""
echo "🚀 To run the web version:"
echo "   cd web"
echo "   python3 -m http.server 8080"
echo "   # Then open http://localhost:8080 in your browser"
echo ""
echo "Or with npx:"
echo "   npx serve web"
