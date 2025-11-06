## エラーの表示例

### 1. 演算子定義のエラー (`error-op-missing-colon.ron`)

**エラー内容**: 演算子定義で `:` が欠けている

**エラー表示**:
```
error: parse failed in operator definition
Error at: op 50 if _ then _ else _
```

実際のエラー箇所を表示する

---

### 2. syntaxブロック内のエラー (`error-syntax-undefined-operator.ron`)

**エラー内容**: 未定義の演算子 `&&` を使用

**エラー表示**:
```
error: parse failed in syntax block (undefined operator or syntax error)
Failing at: [t ::= t && t]
```

syntaxブロック内の具体的なエラー箇所を表示する

---

### 3. ルールヘッドのエラー (`error-rule-head.ron`)

**エラー内容**: ルールヘッドで `=>` が欠けている

**エラー表示**:
```
error: parse failed in rule head
Error at: 1 + 2 3 main
```

ルールヘッドのエラーとして検出して、エラー箇所を表示する

---

### 4. ルールボディのエラー (`error-rule-body.ron`)

**エラー内容**: ルールボディ内で `=>` が欠けている

**エラー表示**:
```
error: parse failed in rule body
Error at: t2 n2 wrongtoken
```

ヘッドとボディを区別したうえで、ボディ内の具体的なエラー行を表示する

---

### 5. 評価時のエラー (`error-evaluation-simple.ron`)

**エラー内容**: mainブロックで必要なルールが定義されていない（`3 + 1` のルールがない）

**エラー表示**:
```
error: evaluation failed in main block

This may indicate:
  - A type error or constraint that cannot be satisfied
  - A syntax error that was not detected during parsing
  - Missing or incorrect rule definitions

Hint: Run with --debug flag for detailed trace:
  swipl ron.pl --debug <file>

Main block attempting to evaluate:
  _=>(_+(3,1),_15076)
```

何を評価しようとして失敗したかを表示する

---

### 6. 閉じ括弧が欠けている (`error-unclosed-brace.ron`)

**エラー内容**: ルールブロックの閉じ括弧 `}` が欠けている

**エラー表示**:
```
error: parse failed in rule body
Error at: main {
```

エラー箇所を表示する
