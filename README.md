# ron

小さい計算体系をなるべくシンプルに定義できるような言語を作ってみたいと思って作りちゅう。

例：ペアノ自然数の足し算

```
# 演算子を宣言
op 50 : S _
op 50 : _ plus _ is _
    
# 規則1
Z plus n is n

# 規則2
(S n1) plus n2 is (S n) {
    n1 plus n2 is n
}

# 問い合わせ
main {
    (S Z) plus (S Z) is x
}
```



例：SKIコンビネータ計算
```
# 演算子を宣言
op 50 : _ -> _
op 50 : _ => _
op 100 : _ _

# 規則1
I x     -> x
K x y   -> x
S x y z -> x z (y z)

# 規則2
x y -> x' y { x -> x' }
x y -> x y' { y -> y' }

# 規則3
x => y { x -> y }
x => y { x -> z; z => y }

# 問い合わせ
main {
    S K S K => x
}
```

例：UTF-8の数学記号を使ってもいい
```
# 論理演算の例
op 10 : _ → _
op 50 : _ ∧ _

# AND 演算
true ∧ true → true
true ∧ false → false
false ∧ true → false
false ∧ false → false

main {
    true ∧ false → x
}
```

例：文法が定義できる
```
# TAPL の Booleans
op 30 : _ -> _
op 30 : _ --> _
op 50 : if _ then _ else _

syntax {
  t ::=                      # terms :
        true                 # constant true
      | false                # constant false
      | if t then t else t   # conditional

  v ::=                      # values :
        true                 # true value
      | false                # false value
}

if true then t2 else t3 -> t2                     # E-IfTrue
if false then t2 else t3 -> t3                    # E-ifFalse
if t1 then t2 else t3 -> if t1' then t2 else t3 { # E-If
  t1 -> t1'
}

t --> t' {
  t -> t'
}
t1 --> t2 {
  t1 -> t3
  t3 --> t2
}

main {
   if (if true then false else true) then false else true --> v
}
```

例：OCaml 風の構文でフィボナッチ関数
```
op 90 : _ _
op 80 : integer _
op 60 : _ + _
op 60 : _ - _
op 60 : _ < _
op 60 : _ >= _
op 50 : _ -> _
op 40 : _ is _
op 40 : _ = _
op 40 : _ \= _
op 30 : _ , _
op 25 : [ _ |- _ ]
op 20 : if _ then _ else _
op 20 : let rec _ _ = _ in _
op 10 : _ |- _ => _   

# c |- e => v は、環境 c で式 e を評価すると値 v になるの意味

# 整数はそのままで値
c |- n => n {
    <integer n>
}

# 足し算と引き算
c |- e1 + e2 => v {
    c |- e1 => v1
    c |- e2 => v2
    <v is v1 + v2>
}
c |- e1 - e2 => v {
    c |- e1 => v1
    c |- e2 => v2
    <v is v1 - v2>
}

# 大小比較
c |- e1 < e2 => true {
    c |- e1 => v1
    c |- e2 => v2
    <v1 < v2>
}
c |- e1 < e2 => false {
    c |- e1 => v1
    c |- e2 => v2
    <v1 >= v2>
}

# if 式
c |- if e1 then e2 else e3 => v {
    c |- e1 => true
    c |- e2 => v
}
c |- if e1 then e2 else e3 => v {
    c |- e1 => false
    c |- e3 => v
}

# 環境と変数
x = v |- x => v
c, x = v |- x => v
c, y = v' |- x => v {
    <x \= y>
    c |- x => v
}

# let rec による関数定義
c |- let rec f x = e1 in e2 => v {
    c, f = [c |- f = x -> e1] |- e2 => v
}

# 関数の適用　
c |- e1 e2 => v { 
    c |- e1 => [c2 |- f = x -> e0]
    c |- e2 => v2
    c2, f = [c2 |- f = x -> e0] , x = v2 |- e0 => v
}

main {
    0 |- let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 9 => v
}
```

## やりたいこと

- 「プログラミング言語の基礎概念」とか TaPL にあるような計算体系を、なるべく簡単に実装できるような言語を作りたい
- やりたいことは Prolog でほぼできるので、見た目をもうちょっと自分好みにしたい。
- 具体的には、
    - mixfix 演算子を定義して自然な文法で書けるようにしたい
    - {} で括って ALGOL 風の見た目にしたい
    - 変数名に小文字を使いたい
    - 変数名の末尾に ' を使って x' みたいに書きたい
    - main を定義したらそこが出発点になるようにしたい

## やっていること

- Prolog でパーズして、そのまま1:1で Prolog の述語に変換して、Prolog の処理系で実行させている

## できていること

- 上記の例は動く。
- UTF-8 の数学記号が使える（例：`→`、`⇒`、`∧`、`∨` など）

## 使い方

```
$ swipl ron.pl example/if.ron
```

## テスト

```
$ bash test.sh
```