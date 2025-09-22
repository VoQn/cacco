# Serena プロジェクトインデックス作成

現在のプロジェクトをインデックス化して Serena のツールパフォーマンスを向上させます。

```bash
# Load Serena configuration
eval $(cat .serena/.serenarc 2>/dev/null | grep -v '^#' | grep -v '^$')
cd ${SERENA_ROOT:-/path/to/local/serena-repo} && uv run serena project index
```

**説明**: .serenarc から環境変数を読み込み、セキュアに設定されたローカル Serena リポジトリパスを使用してプロジェクトをインデックス化。
