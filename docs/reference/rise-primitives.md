---
id: rise-primitives
title: RISE Primitives
sidebar_label: RISE Primitives
slug: /reference/rise-primitives
---

## Type syntax cheatsheet

**Note**: curly braces quantify over implicit parameters.

Type | Syntax
-----|-------
Function from `A` to `B` | `(A -> B)`
Dependent function from `a : A` to `B a` | `(x : A -> B)`
Dependent function application | `f(n)`
Vector of `A` of size `n` | `<n>A`
Array of `A` of size `n` | `n.A`
Dependent array of `F : Nat -> A` of size `n` | `n..F`
Index of size `n : Nat` | `idx[n]`
Pair of `A` and `B` | `(A, B)`
Dependent pair of `x : A` and `B a` | `(x : A ** B)`

## Primitives

```scala mdoc:passthrough
    val primTypeScheme = os.pwd / os.RelPath("src/main/scala/rise/core/primitives/primitives.rise")
    val output = Seq("```", os.read(primTypeScheme), "```").mkString("\n")
    println(output)
```