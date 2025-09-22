# Serena MCP Server 起動

ローカルのパッチ済み Serena リポジトリを使用して Haskell LSP を有効化した状態で起動します。

```bash
# Load Serena configuration
eval $(cat .serena/.serenarc 2>/dev/null | grep -v '^#' | grep -v '^$')
cd ${SERENA_ROOT:-/path/to/local/serena-repo} && uv run serena start-mcp-server --context ide-assistant --project $(pwd)
```

**説明**: .serenarc から環境変数を読み込み、セキュアに設定されたローカル Serena リポジトリパスを使用して起動。Haskell LSP パッチが適用済み。
