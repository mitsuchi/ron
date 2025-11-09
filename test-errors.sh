#!/bin/bash

# エラー表示のテストスクリプト

echo "======================================"
echo "エラー表示の改善テスト"
echo "======================================"
echo ""

# エラーケース1
echo "【1】演算子定義で : が欠けている"
echo "--------------------------------------"
swipl ron.pl example/error-op-missing-colon.ron 2>&1 | head -5
echo ""

# エラーケース2
echo "【2】syntaxブロック内で未定義の演算子を使用"
echo "--------------------------------------"
swipl ron.pl example/error-syntax-undefined-operator.ron 2>&1 | head -5
echo ""

# エラーケース3
echo "【3】ルールヘッドの構文エラー"
echo "--------------------------------------"
swipl ron.pl example/error-rule-head.ron 2>&1 | head -5
echo ""

# エラーケース4
echo "【4】ルールボディの構文エラー"
echo "--------------------------------------"
swipl ron.pl example/error-rule-body.ron 2>&1 | head -5
echo ""

# エラーケース5
echo "【5】評価時のエラー（ルールが見つからない）"
echo "--------------------------------------"
swipl ron.pl example/error-evaluation-simple.ron 2>&1 | head -15
echo ""

# エラーケース6
echo "【6】閉じ括弧が欠けている"
echo "--------------------------------------"
swipl ron.pl example/error-unclosed-brace.ron 2>&1 | head -5
echo ""

# エラーケース7
echo "【7】未定義の非終端記号を参照"
echo "--------------------------------------"
swipl ron.pl example/error-undefined-nonterminal.ron 2>&1 | head -10
echo ""

echo "======================================"
echo "曖昧性チェックのテスト (--check)"
echo "======================================"
echo ""

# 曖昧性チェック1
echo "【1】カンマ演算子の競合"
echo "--------------------------------------"
swipl ron.pl --check example/test_ambiguity.ron 2>&1 | grep -A 1 "warning"
echo ""

# 曖昧性チェック2
echo "【2】if-then と if-then-else の競合"
echo "--------------------------------------"
swipl ron.pl --check example/test_ambiguity2.ron 2>&1 | grep -A 1 "warning"
echo ""

# 曖昧性チェック3
echo "【3】関数適用と他の演算子の競合"
echo "--------------------------------------"
swipl ron.pl --check example/test_ambiguity3.ron 2>&1 | grep -A 1 "warning"
echo ""

# 曖昧性チェック4
echo "【4】曖昧性なし（正常ケース）"
echo "--------------------------------------"
echo "期待: 警告なしで実行"
swipl ron.pl --check example/arrow1.ron 2>&1 | head -3
echo ""

echo "======================================"
echo "すべてのテストケースを表示しました"
echo "======================================"

