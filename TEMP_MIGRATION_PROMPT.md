## 目的

Stack/hpack を用いる Haskell プロジェクト cacco を、最新の Stackage LTS に適合させる。ビルド/テストをグリーン維持しつつ、警告削減と CI/フォーマッタ運用も整える。

## 前提/制約

- 現状は Stack+hpack（package.yaml）、fourmolu 導入済み（.fourmolu.yaml, Makefile: fmt/fmt-check）。CI は circle.yml。
- 既に Prettyprinter へ移行済み。Hi 系は Higher-Order-Functors 設計（参考: [Encoding Higher-Order Effects in Haskell](https://www.casperbp.net/blog/posts/2023-08-encoding-higher-order-effects/index.html)）。
- 挙動変更を避け、最小差分で対応。新たな警告は可能な範囲で解消。
- 以降の作業はブランチで実施し、コミットはステップごとに小さく分割。

## やってほしいこと（順序）

1) ブランチ作成

- feat/upgrade-lts-<日付> などの名前で作成。

2) 最新 LTS の決定と適用

- 最新 LTS の取得:

```bash
stack ls snapshots --remote lts | tail -1
```

- `stack.yaml` の `resolver` を最新 LTS に更新。

3) 依存解決（package.yaml）

- 依存の先行取得:

```bash
stack build --only-dependencies --no-terminal
```

- 競合/欠落があれば `package.yaml` の上限/下限を調整、必要なら `extra-deps` 追加（ただし極力回避）。
- 依存のメジャーアップによる API 変更が出た場合は最小修正で対応。

4) ビルド/テスト

```bash
stack build --no-terminal
stack test  --no-terminal
```

- 失敗時はエラー原因別に最小修正。GHC の言語拡張/Kind 注釈/Deprecated API 等に起因するものは安全に直す。

5) 警告削減

- 既存の `-Wall -Wcompat` で出る主要警告を可能な範囲で解消（未使用インポート、非網羅パターン、古い `*` Kind など）。
- fourmolu を用いた整形チェック:

```bash
make fmt-check
# 必要に応じて
make fmt
```

- CI（circle.yml）で fmt-check が通ること。

6) CI の確認

- circle.yml で最新 LTS でも通ることを確認。必要なら fourmolu インストール手順（`stack build fourmolu --copy-compiler-tool`）を残す。

7) 仕上げ

- 変更要約（何を/なぜ）を PR 説明に記載。
- 各コミットは意味単位（resolver 更新、依存調整、コード最小修正、警告解消、CI/フォーマッタ調整）で分割。

## 受け入れ基準（Done の定義）

- 最新 LTS に更新されている（stack.yaml）。
- `stack build`/`stack test` がローカルでグリーン。
- CI（circle.yml）がグリーン。
- フォーマッタ `make fmt-check` が成功。
- 新規の重大な警告なし（残る場合は理由を PR で明記）。
- 動作仕様の変更なし（テストで担保）。

## 作業上の補助/方針

- 依存の大幅な上げ幅が必要な場合、まずは LTS 追随を優先し、API 変更は最小限のアダプタで対処。
- 既存スタイル/設計（Prettyprinter、Hi 系、Ix 系）を尊重する。
- 変更は小さく安全に。コミットメッセージは Conventional Commits 相当を推奨（例: build:, chore:, fix:, refactor:, test: など）。
- 必要に応じて次を活用して調査/影響範囲把握:
  - `stack ls snapshots --remote lts`
  - serena（定義参照/参照元調査）
  - 既存テストの失敗箇所からのボトムアップ修正

まずは最新 LTS の特定→ resolver 更新→ 依存解決→ ビルド/テスト実行まで進めてください。

## Serena (MCP) の使い方メモ

このリポジトリでは、コードリーディング/参照探索のために MCP サーバー "serena" が利用できます。新規チャットのエージェントが気づかない可能性があるため、以下を参照してください。

- 有効化
  - プロジェクトを "cacco" で有効化（activate_project）。
  - 現在設定の確認（get_current_config）で Active project が `cacco` になっていることを確認。

- 代表的な機能
  - search_for_pattern: 正規表現で横断検索（paths_include_glob/paths_exclude_glob で範囲絞り込み可）。
    - 例: `substring_pattern: "Data\\.Text\\.Prettyprint\\.Doc"`, `paths_include_glob: "**/*.hs"`。
  - get_symbols_overview: 単一ファイルのシンボル一覧（トップレベル）を取得。
    - 例: `relative_path: "src/Cacco/Syntax/AST.hs"`。
  - find_symbol: シンボルを name_path で検索。`depth` で子要素を列挙。
    - 例: `name_path: "AstF"`, `relative_path: "src/Cacco/Syntax/AST.hs"`, `depth: 1`。
  - find_referencing_symbols: 指定ファイル内の対象シンボルを参照する側を列挙。
    - 例: `name_path: "Cacco.Syntax.AST/AstF"`, `relative_path: "src/Cacco/Syntax/AST.hs"`。

- ベストプラクティス
  - 広くあたりをつけるときは search_for_pattern を使い、対象が見えたら get_symbols_overview / find_symbol で深掘り。
  - パターン検索は `paths_include_glob` でノイズを抑える（例: `src/**/*.hs`, `test/**/*.hs`）。
  - 大規模変更時は、参照探索（find_referencing_symbols）で影響範囲を先に把握してから実装。

> 補足: serena の応答でファイルパス/シンボルが分かったら、通常のエディタ編集や Stack ビルド/テストに直結してください。
