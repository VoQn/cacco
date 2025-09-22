# Cacco プロジェクト概要

## プロジェクト情報
- **名前**: Cacco
- **バージョン**: 0.1.0.0
- **言語**: Haskell
- **状態**: 設計段階（まだ動作しない）
- **作者**: Kazuhiro Mizushima

## プロジェクト構造
- **メインアプリケーション**: `app/Main.hs` - REPLループとコマンドライン引数処理
- **ライブラリ**: `src/` - コア機能
- **テスト**: `test/` - Tasty フレームワークを使用

## 主要モジュール
- `Cacco.Core` - 組み込み関数（eq, ne, gt, ge, lt, le, add, sub, mul等）
- `Cacco.REPL` - 対話式環境
- `Cacco.Eval` - 式の評価
- `Cacco.Syntax.AST` - 抽象構文木
- `Cacco.Val` - 値の表現（Unit, Bool, 数値型, Text, List等）
- `Cacco.Syntax.Parser` - パーサー

## 言語特徴
- S式ベースの構文
- ファジー型システム（Fuzzy Numeric, Fuzzy Integer等）
- 型推論
- REPL環境

## 技術スタック
- Haskell Stack
- Megaparsec（パーサー）
- Lens（レンズ）
- Recursion-schemes（再帰スキーム）
- Prettyprinter（プリティプリント）