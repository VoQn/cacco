# Serena Haskell 開発モード起動

ローカルのパッチ済み Serena リポジトリを使用して、planning モードで起動します。

```bash
# Load Serena configuration
eval $(cat .serena/.serenarc 2>/dev/null | grep -v '^#' | grep -v '^$')
cd ${SERENA_ROOT:-/path/to/local/serena-repo} && uv run serena start-mcp-server --context ide-assistant --project $(pwd) --mode planning
```

**説明**: .serenarc から環境変数を読み込み、セキュアに設定されたローカル Serena リポジトリパスを使用して、Haskell 開発に最適化された planning モードで起動。
