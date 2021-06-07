---
title: RISE Type System
sidebar_label: RISE Type System
---

## Well-formed Types


```scala mdoc:passthrough
println(s"""
### Kinds
$$
${rise.core.types.latex.wellFormedTypes.kinds}
$$

### Kinding Structural Rule
$$
${rise.core.types.latex.wellFormedTypes.structuralRules}
$$

### Type Equality
$$
${rise.core.types.latex.wellFormedTypes.typeEquality}
$$

### Address Spaces
$$
${rise.core.types.latex.wellFormedTypes.addressSpaces}
$$

### Nat to Nat Type Level Functions
$$
${rise.core.types.latex.wellFormedTypes.natToNatTypeLevelFunctions}
$$

### Nat to Data Type Level Functions
$$
${rise.core.types.latex.wellFormedTypes.natToDataTypeLevelFunctions}
$$

### Natural numbers
$$
${rise.core.types.latex.wellFormedTypes.naturalNumbers}
$$

### Data Types
$$
${rise.core.types.latex.wellFormedTypes.dataTypes}
$$

### Types
$$
${rise.core.types.latex.wellFormedTypes.types}
$$
""")
```

## Typing Rules

```scala mdoc:passthrough
println(s"""
### Structural Rules
$$
${rise.core.types.latex.typingRules.structural}
$$

### Introduction and Elimination Rules
$$
${rise.core.types.latex.typingRules.introAndElim}
$$
""")
```
