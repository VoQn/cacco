#!/bin/bash

set -euo pipefail

# Serena MCP Server 起動スクリプト
# ローカルのパッチ済み Serena リポジトリを使用して Haskell LSP を有効化した状態で起動します。

# ヘルプメッセージ
show_help() {
    cat << EOF
Serena MCP Server 起動

ローカルのパッチ済み Serena リポジトリを使用して Haskell LSP を有効化した状態で起動します。

使用方法:
    $0 [オプション]

オプション:
    -h, --help      このヘルプメッセージを表示
    -v, --verbose   詳細な出力を表示

環境変数:
    SERENA_ROOT     Serena リポジトリのパス（.serena/.serenarc で設定）

EOF
}

# 引数解析
VERBOSE=false
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        *)
            echo "Error: 不明なオプション: $1" >&2
            show_help
            exit 1
            ;;
    esac
done

# 環境変数の安全な読み込み
if [ "$VERBOSE" = true ]; then
    echo "Loading Serena configuration..."
fi

set -a
source .serena/.serenarc 2>/dev/null || {
    echo "Warning: .serena/.serenarc not found. Please set SERENA_ROOT." >&2
}
set +a

# 現在のプロジェクトディレクトリを取得
PROJECT_DIR=$(pwd)

# SERENA_ROOT の確認とコマンド実行
if [ -n "${SERENA_ROOT:-}" ] && [ -d "${SERENA_ROOT}" ]; then
    if [ "$VERBOSE" = true ]; then
        echo "Starting Serena MCP server..."
        echo "Project directory: $PROJECT_DIR"
        echo "Serena repository: $SERENA_ROOT"
    fi

    cd "${SERENA_ROOT}" && uv run serena start-mcp-server --context ide-assistant --project "$PROJECT_DIR"
else
    echo "Error: SERENA_ROOT not set or directory does not exist. Please configure .serena/.serenarc." >&2
    echo "Example .serena/.serenarc content:" >&2
    echo "SERENA_ROOT=/path/to/local/serena-repo" >&2
    exit 1
fi
