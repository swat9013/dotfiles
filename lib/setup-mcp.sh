#!/bin/bash
#
# MCP Server & Plugin Setup Script for Claude Code
# Idempotent: Only adds servers/plugins that don't exist
#

set -euo pipefail

# Check if claude command exists
if ! command -v claude &> /dev/null; then
    echo "Error: claude command not found"
    exit 1
fi

# ============================================
# MCP Server Functions
# ============================================

# Function to check if MCP server exists
mcp_exists() {
    local name="$1"
    claude mcp get "$name" > /dev/null 2>&1
}

# Function to add MCP server (idempotent)
add_mcp_server() {
    local name="$1"
    shift

    if mcp_exists "$name"; then
        echo "  ✓ $name (already configured)"
    else
        if claude mcp add --scope user "$name" "$@" > /dev/null 2>&1; then
            echo "  ✓ $name (added)"
        else
            echo "  ✗ $name (failed to add)"
            return 1
        fi
    fi
}

# ============================================
# Plugin Functions
# ============================================

# Function to check if marketplace exists
marketplace_exists() {
    local name="$1"
    claude plugin marketplace list 2>/dev/null | grep -q "❯ ${name}$"
}

# Function to add marketplace (idempotent)
add_marketplace() {
    local name="$1"
    local source="$2"

    if marketplace_exists "$name"; then
        echo "  ✓ $name (already configured)"
    else
        if claude plugin marketplace add "$source" > /dev/null 2>&1; then
            echo "  ✓ $name (added)"
        else
            echo "  ✗ $name (failed to add)"
            return 1
        fi
    fi
}

# Function to install plugin (idempotent - install command is already idempotent)
install_plugin() {
    local name="$1"

    if claude plugin install "$name" --scope user > /dev/null 2>&1; then
        echo "  ✓ $name (installed/verified)"
    else
        echo "  ✗ $name (failed to install)"
        return 1
    fi
}

# ============================================
# Setup
# ============================================

echo "Setting up MCP servers..."
echo ""

# HTTP servers
add_mcp_server "deepwiki-http" \
    --transport http \
    "https://mcp.deepwiki.com/mcp"

# Stdio servers
add_mcp_server "serena" \
    -- uvx \
    --from "git+https://github.com/oraios/serena" \
    serena \
    start-mcp-server \
    --context ide-assistant \
    --project '$(pwd)'

echo ""
echo "Setting up plugin marketplaces..."
echo ""

# Marketplaces
add_marketplace "thedotmack" "thedotmack/claude-mem"

echo ""
echo "Installing plugins..."
echo ""

# Plugins
install_plugin "claude-mem"

echo ""
echo "Done."
