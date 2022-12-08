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
x y -> x’ y { x -> x’ }
x y -> x y’ { y -> y’ }

# 規則3
x => y { x -> y }
x => y { x -> z; z => y }

# 問い合わせ
main {
    S K S K => x
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
op 40 : _ \\= _
op 30 : _ , _
op 25 : [ _ |- _ ]
op 20 : if _ then _ else _
op 20 : let rec _ _ = _ in _
op 10 : _ |- _ => _
    
c |- n => n {
    <integer n>
}
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
c |- if e1 then e2 else e3 => v {
    c |- e1 => true
    c |- e2 => v
}
c |- if e1 then e2 else e3 => v {
    c |- e1 => false
    c |- e3 => v
}

x = v |- x => v
c, x = v |- x => v
c, y = v' |- x => v {
    <x \\= y>
    c |- x => v
}

c |- let rec f x = e1 in e2 => v {
    c, f = [c |- f = x -> e1] |- e2 => v
}
c |- e1 e2 => v { 
    c |- e1 => [c2 |- x = y -> e0]
    c |- e2 => v2
    c2, x = [c2 |- x = y -> e0] , y = v2 |- e0 => v
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
- ただし # でのコメントはまだ動かない