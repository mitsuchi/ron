#!/bin/bash

# 単一ファイルのテスト関数
test_ron_file() {
    local file="$1"
    local expected_output="$2"
    
    echo "Testing $file..."
    
    # swipl で実行し、出力をキャプチャ
    local output=$(swipl ron.pl "$file" 2>/dev/null)
    
    # 期待する出力が含まれているかチェック
    if echo "$output" | grep -q "$expected_output"; then
        echo "✓ PASS: $file"
        return 0
    else
        echo "✗ FAIL: $file"
        echo "  Expected: $expected_output"
        echo "  Got: $output"
        return 1
    fi
}

# 複数行の出力をテストする関数
test_ron_file_multiline() {
    local file="$1"
    shift
    local expected_outputs=("$@")
    
    echo "Testing $file..."
    
    # swipl で実行し、出力をキャプチャ
    local output=$(swipl ron.pl "$file" 2>/dev/null)
    
    # すべての期待する出力が含まれているかチェック
    local all_found=true
    for expected in "${expected_outputs[@]}"; do
        if ! echo "$output" | grep -q "$expected"; then
            echo "✗ FAIL: $file"
            echo "  Missing expected output: $expected"
            echo "  Got: $output"
            all_found=false
            break
        fi
    done
    
    if [ "$all_found" = true ]; then
        echo "✓ PASS: $file"
        return 0
    else
        return 1
    fi
}

# グローバル変数
PASS=0
FAIL=0

# テスト用のヘルパー関数
run_test() {
    if test_ron_file "$1" "$2"; then
        ((PASS++))
    else
        ((FAIL++))
    fi
}

# 複数行テスト用のヘルパー関数
run_test_multiline() {
    if test_ron_file_multiline "$@"; then
        ((PASS++))
    else
        ((FAIL++))
    fi
}

# 全ファイルのテスト
test_all() {
    echo "Running tests on example/*.ron files..."
    echo "========================================"
    
    # 各ファイルをテスト
    run_test "example/add.ron" "x = 10"
    run_test "example/and.ron" "v = false"
    run_test "example/arrow1.ron" "x = 2"
    run_test "example/arrow2.ron" "x = 3"
    run_test "example/arrow3.ron" "x = 3"
    run_test "example/cont.ron" "v = 111"
    run_test "example/dcont.ron" "v = 14"
    run_test "example/if.ron" "v = 0"
    run_test "example/letcc.ron" "v = 101"
    run_test "example/logic.utf8.ron" "x = false"
    run_test "example/ml-peano.ron" "v = S S S S S S S S S S S S S Z"
    run_test "example/ocaml.ron" "v = 34"
    run_test "example/or.ron" "x = false"
    run_test "example/plus.ron" "x = S S Z"
    run_test_multiline "example/plus10.ron" "x = 30" "y = 60"
    run_test "example/plus2.ron" "x = S S S 0"
    run_test "example/ski.ron" "x = K"
    run_test "example/ski.utf8.ron" "x = K"
    run_test "example/succ.ron" "x = succ succ 0"
    run_test "example/syntax-without-macro.ron" "v = true"
    run_test "example/syntax.ron" "v = true"
    run_test "example/tapl.arithmetic.ron" "v = succ 0"
    run_test "example/tapl.booleans.ron" "v = true"
    run_test "example/tapl.booleans.utf8.ron" "v = true"
    run_test "example/type.ron" "t = int"
    run_test "example/fspl.imp.ron" "v = φ : p = 0 : q = 55"
    run_test "example/evaluation-context.add.ron" "x = 10"
    run_test "example/evaluation-context.add.left-first.ron" "x = 3 + (3 + 4)"
    run_test "example/evaluation-context.add.right-first.ron" "x = 1 + 2 + 7"
    run_test "example/evaluation-context.add.recursive.ron" "x = 10"
    run_test "example/lambda-full.ron" "w = & b . b"
    run_test "example/lambda-full-context.ron" "w = & b . b"
    run_test "example/lambda-call-by-value.ron" "v = & b . (& a . a) b"
    run_test "example/lambda-call-by-name.ron" "v = & b . (& a . a) b"
    run_test "example/lambda.ron" "w = λ b . (λ a . a) b"
    run_test "example/tapl.typed-booleans.ron" "T = Bool"
    run_test "example/tapl.typed-arithmetic.ron" "T = Nat"
    run_test "example/tapl.simply-typed-lambda.ron" "T = Bool -> Bool"
    run_test "example/tapl.base-type.ron" "T = (A -> A) -> A -> A"
    run_test "example/tapl.unit.ron" "w = λ d : Unit . d"
    run_test "example/use-percent-for-newline.ron" "x = 2"
    run_test "example/tapl.ascription.ron" "w = unit"
    run_test "example/tapl.let.ron" "w = true"
    run_test "example/use-do-end-for-block.ron" "x = 2"
    run_test "example/multiple_use.ron" "x = 2"
    run_test "example/tapl.typed-lambda.arith.ron" "w = succ succ succ 0"
    run_test "example/tapl.pair.ron" "v = succ 0"
    run_test "example/tapl.tuple.ron" "v = succ 0"
    run_test "example/tapl.record.ron" "T = Nat"
    run_test "example/tapl.record.pattern.ron" "v = { succ 0 , true }"
    run_test_multiline "example/tapl.simple.full.ron" \
                       "v1 = succ succ succ 0" \
                       "v2 = 0" \
                       "v3 = succ 0" \
                       "v4 = λ d : Unit . d" \
                       "v5 = true" \
                       "v6 = succ 0"

    echo "========================================"
    echo "Results: $PASS passed, $FAIL failed"
    
    if [ $FAIL -eq 0 ]; then
        echo "All tests passed!"
        return 0
    else
        echo "Some tests failed!"
        return 1
    fi
}

# メイン実行
case "${1:-all}" in
    "and")
        test_and
        ;;
    "all")
        test_all
        ;;
    *)
        echo "Usage: $0 [and|all]"
        echo "  and: Test only example/and.ron"
        echo "  all: Test all example/*.ron files"
        exit 1
        ;;
esac