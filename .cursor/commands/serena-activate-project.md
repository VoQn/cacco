# Serena プロジェクト有効化

現在のプロジェクトを Serena で有効化します。

```bash
# Load Serena configuration
eval $(cat .serena/.serenarc 2>/dev/null | grep -v '^#' | grep -v '^$')
cd ${SERENA_ROOT:-/path/to/local/serena-repo} && uv run serena activate_project --project $(pwd)
```

**説明**: .serenarc から環境変数を読み込み、セキュアに設定されたローカル Serena リポジトリパスを使用して現在のプロジェクトを有効化。
