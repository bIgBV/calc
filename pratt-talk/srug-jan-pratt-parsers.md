# Pratt parsing

Bhargav Voleti

[bIgBV](https://github.com/bIgBV)

* Repo: [calc](https://github.com/bIgBV/calc)

---

# Parsing?

* Go from text -> [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
* Part of compilers and interpreters
* See the `rustc`: [HIR](https://rustc-dev-guide.rust-lang.org/hir.html), [MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html)

![right 25%](ast.png)

```
while b ≠ 0:
    if a > b:
        a := a - b
    else:
        b := b - a
return a
```

^ General notes on what an AST is and how it represents the program text in such a way to perform other transoformations and optimizations to the code.

---

# Pratt parser

* Combine recursion + explicit precedence
* Simpler (imo) structure

^ Go to `parse_expression` function in the code to talk through how the method is set up to always parse a the prefix expression and then the infix. Focus on the recursion part first. We'll talk about precedence later.

---

# Parsing functions

* Responsible for parsing individual parts
* Unary, Binary, Identifiers, Strings, Blocks, functions, etc..

^ go to the parsing functions in the code to show how each function is responsible for it's own unit if functionality.

---

# Lookup table

* The core of the parser
* Maps tokens to parse functions

^ The parsing mapping in the code and talking about how each token type will have a different parse function based on whether we're trying to parse the prefix of an expression or an infix expression

---

# Associativity

* When to stop?
* How to associate correctly?
* `a - b - c => ((a - b) - c)`

^ I need to talk about how operatosr have associativity, meaning, in this case the `-` is right associative, so we want the first `-` to bind `a` & `b` together into an expression and _then_ `c`

---

# Precedence based parsing

* Precedence for every token
* Only parse when `preNext` > `preCurr`

^ back to `parse_expression` again, but this time, focus on how the precedence affects the logic of the method.

---

# Example

* Example expression `1 + 2`

^ Walk trough the entire flow of the logic when parsing the expression and what the result looks like