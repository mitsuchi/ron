# ron プロジェクト Makefile

.PHONY: test test-simple clean help

# デフォルトターゲット
help:
	@echo "Available targets:"
	@echo "  test        - Run all tests (using test.pl)"
	@echo "  test-simple - Run simple regression tests"
	@echo "  clean       - Clean temporary files"
	@echo "  help        - Show this help message"

# 全テストを実行
test:
	@echo "Running comprehensive tests..."
	swipl -g "run_all_tests." -t halt test.pl

# シンプルなregression testを実行
test-simple:
	@echo "Running simple regression tests..."
	swipl simple_test.pl

# 掃除
clean:
	@echo "Cleaning temporary files..."
	find . -name "*.tmp" -delete
	find . -name "*~" -delete

# 新しい期待値ファイルを生成（開発者向け）
generate-expected:
	@echo "Generating expected output files..."
	swipl ron.pl example/plus.ron | tail -1 > example/plus.ron.expected
	swipl ron.pl example/if.ron | tail -1 > example/if.ron.expected
	swipl ron.pl example/ski.ron | tail -1 > example/ski.ron.expected