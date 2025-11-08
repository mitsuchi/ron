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
echo "すべてのエラーケースを表示しました"
echo "======================================"

