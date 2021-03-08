---
title: A tutorial of the RISE language, and the Shine compiler
sidebar_label: Tutorial of RISE and Shine
---
```scala mdoc:invisible
import elevate.core._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types._
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.traversal._
import util.gen
```

Starting from a High-Level RISE Program, and an ELEVATE Optimization
Strategy the Shine compiler rewrites the high-level program as specified
by the optimization strategy into a Low-Level RISE Program that encodes 
all implementation and optimization decisions explicitly. 
      
The code generator processes the low-level program to generate
the final Optimized C, OpenMP, OpenCL, or CUDA Program.

## High-Level Program
This is an example of a high-Level program written in RISE.
The shown example is the multiplication of a `nxk`-matrix called `A`
and a mxk-matrix called `B`.
```scala mdoc:silent
val highLevelProgram: ToBeTyped[Rise] =
    depFun((n: Nat, m: Nat, k: Nat) =>
      fun(n`.`k`.`f32)(A => fun(k`.`m`.`f32)(B =>
        A |> map(fun(rowOfA =>
          B |> transpose |> map(fun(colOfB =>
            zip(rowOfA)(colOfB) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(l(0.0f)) )) )) )) )
```

The matrix dimensions are represented as part of the type of the matrices:
 - the type of matrix `A` is `n.k.f32`
 - the type of matrix `B` is `k.m.f32`
The identifiers used in the type (here: `n`, `m`, and `k`) are introduced
and scoped by `depFun`

The matrix values are introduced and scoped as function parameters by two
nested `fun`s

The body of the nested functions represents the computation of the matrix
matrix multiplication:
 - two nested `map` primitives apply the dot product to each combination
   of a `rowOfA` and a `colOfB`
 - the dot product computation is represented by a composition of the
   `zip`, `map`, and `reduce` primitives

We often prefer the pipe notation `(x |> f)` over the equivalent function
call notation `f(x)` as it allows expressions to be read from
left-to-right and top-to-bottom.

Primitives (such as `map`, `transpose`, `zip`, and `reduce`) are functions
with types that explain their possible usage and with a clearly defined
denotational semantics:
 - `[x1, ..., xn] |> map(f) == [f(x1), ..., f(xn)]`
 - `[ [x11, ...., x1n], ..., [xm1, ..., xmn] ] |> transpose
       == [ [x11, ...., xm1], ..., [x1n, ..., xmn] ]`
 - `zip([x1, ..., xn])([y1, ..., yn]) == [(x1, y1), ..., (xn, yn)]`
 - `[x1, ..., xn] |> reduce(op)(init) == init op x1 op ... op xn`

The resulting RISE expression has the Scala type `ToBeTyped[Rise]`
On conversion to the underlying Scala type `Rise` type inference will be
performed automatically.

We can easily print the internal representation of the high-level program:
```scala mdoc
println(highLevelProgram.toExpr)
```

## Optimization Strategy
This is an example of an optimization strategy written in ELEVATE.
It describes that the outermost map computation will be performed in
parallel as well as that the nested map computation and the reduction
will be performed sequentially.

```scala mdoc:silent
val optimizationStrategy: Strategy[Rise] =
    (`map -> mapPar`       `@` outermost(isPrimitive(map)))  `;`
    (`map -> mapSeq`       `@` outermost(isPrimitive(map)))  `;`
    (`reduce -> reduceSeq` `@` everywhere)
```

The shown example demonstrates one possible way to rewrite the high-level
RISE program above into a low-level RISE program from which code can be
generated.

Strategies in ELEVATE are functions with the following specific type:
```scala
    type Strategy[P] = P => RewriteResult[P]
```

The return type `RewriteResult[P]` indicates the two possible outcomes of
applying a rewrite strategy to a program of type `P`: either the program
has been successfully rewritten, or the rewrite strategy failed.

Strategies in ELEVATE are written as compositions of smaller strategies.

The simplest strategies are rewrite rules that replace an expression
with another expression. An example of such a rule is the `map |-> mapPar`
strategy that replaces an occurrence of the `map` primitive with the
`mapPar` primitive indicating that the computation of the map should be
performed in parallel.

The `outermost` and `everywhere` strategies are examples of traversals
that describe where other strategies should be applied.
We can use the `@` notation to compose them as shown in the example.

We can also easily print the internal representation of the
optimization strategy:
```scala mdoc
println(optimizationStrategy)
```

This alternative strategy explicitly fused the innermost map and reduce
patterns. It then turns every remaining map into a sequential map, and the
remaining reduce into a sequential reduction.
```scala mdoc:silent
val anotherOptimizationStrategy: Strategy[Rise] =
    (`map >> reduce -> reduce` `@` everywhere) `;`
    (`map -> mapSeq`           `@` everywhere) `;`
    (`reduce -> reduceSeq`     `@` everywhere)
```

This strategy vectorizes the computation of the innermost map pattern
that itself will be performed sequentially and stores its temporary output
as vectors. It will then use the optimization and implementation decisions
described in the initial optimization strategy.
```scala mdoc:silent
val yetAnotherOptimizationStrategy: Strategy[Rise] =
    innermost(isAppliedMap)(
      `map(f) -> asVector >> map(f_vec) >> asScalar`(4) `;`
      (`map -> mapSeq` `@` innermost(isPrimitive(map))) `;`
      storeTempAsVectors
    ) `;`
    optimizationStrategy
```

## Rewriting
This function performs the rewriting by applying the given
optimization strategy to the given program.
```scala mdoc:silent
def rewriting(program: Rise, strategy: Strategy[Rise]): Rise = {
    // we know that the shown strategies will always succeed, therefore, it is
    // ok to unwrap the final RewriteResult using .get
    strategy(program).get
  }
```

## Low-Level Program
This is the low-level RISE program that is produced by rewriting the
high-level program using one of the optimization strategies
```scala mdoc:silent
val lowLevelProgram: Rise =
    rewriting(highLevelProgram, optimizationStrategy)
```

## Code Generation
This function performs the code generation translating the given
low-level program to optimized code.
```scala mdoc:silent
def codeGeneration(program: Rise): String = {
    gen.openmp.function.asStringFromExpr(program)
    // similar API for generating C or OpenCL code exist:
    // gen.c.function.asStringFromExpr(program)
    // gen.opencl.kernel.asStringFromExpr(program)
  }
```

## Optimized Program
The final optimized program in C, OpenMP, or OpenCL.
```scala mdoc
val optimizedProgram: String =
    codeGeneration(lowLevelProgram)
```
