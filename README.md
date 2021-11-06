# JARE: Just Another Regular Expression Engine

In this project we implement a fully-featured formal regular expression engine. We emphasize the qualifier "formal" in the previous sentence for two reasons. First, unlike POSIX-like pattern-matching engines ("regex"), backtracking is disallowed in our model—this is because this feature violates the regularity of the language and thus makes the theoretical foundation upon which our matcher is based (e.g. the Brzozowski derivative) inaccessible. Secondly, our system offers a range of very useful and theoretically-sound features, such as complement matching (i.e. `~r`) and logical `and` (i.e. `r&s`), which are typically not available in POSIX-like engines.        

## Grammar

We use the standard set-theoretic definition for regular expressions. In particular, for a given alphabet `Σ` we define the instances of a regular expression, `r` and `t`, with the following linear grammar:

```
r,s := ∅        # Empty Language
    | ε         # Empty String
    | a         # Literal a ∈ Σ 
    | rs        # Concatenation
    | r*        # Kleene-closure
    | r+s       # Logical Or
    | r&s       # Logical And
    | ~r        # Complement
```

We refer to the terms of this grammar as the "core terms". Unless otherwise specified, we assume our alphabet is equal to the Unicode character set for all purposes with the exception of character classes which are defined only for Ascii characters. Lastly, we use the shorthand `rⁿ` to denote the repetition of the expression `n` times and we interpret `r⁰ = ε` for all `r ≠ ∅`.  


Additionally, we define the following short-hand operators ("sugar"), which are theoretically reduced to a sequence of core terms but their implementation makes use of specialized internal data structures for optimized performance (see below):    

```
rᕀ := rr*   # Positive Kleene-closure
r? := r+ε   # Optional Matching

# Range Quantifiers:
r{,} := r*
r{m,} := rᵐ + rᵐᕀ¹ + ...
r{,n} := r⁰ + r¹ + ... + rⁿ
r{m,n} := rᵐ + rᵐᕀ¹ + ... + rⁿ  ∀ m ≤ n
```

We also define a minimal set of character classes. We only implement a subset of the standard POSIX character classes, but our infrastructure is very flexible and adding more classes is trivial (see `src/Alphabet.elm`). First, we define the character range function as follows:

```
Φ(a,b) := {c ∈ Σ: a ˧ c and c ˧ b}
```
where `˧` is the partial-order relation over `Σ`. Given the range function we define these (named) classes:

```
\d := Φ(0,9) = {0,1,..,9}
\D := Σ* - \d
\w := Φ(a,z) ⋃ Φ(A,Z) ⋃ \d
\W := Σ* - \w
\s := {' ', \t, \n}
\S := Σ* - \s

class := Σ  # Wild Card, also aliased as '.'
      | \d  # Digit
      | \D  # Non-digit
      | \w  # Alphanumeric
      | \W  # Non-alphanumeric
      | \s  # White space
      | \S  # None white-space
      | Φ(a,b)   ∀ a,b ∈ Σ, a ˧ b  

for all classes c and t:
[ct] := c + t
[^ct] := ~(c + t)
```

## Written Syntax

The written syntax of our regular expressions follows the grammar defined above with the following exceptions:

- The alternation (`or`) symbol is `|` instead of `+`. Consequently, we use `+` to denote the positive Kleene-closure.
- We write the range function `Φ(a,b)` as `a-b`.
- We use the backslash `\` to escape reserved characters, as in `\+` and `\ε`.

## Matching

We use the Brzozowski derivative to test the membership of a string `s ∈ Σ*` in the language of a regular expression `r`. In the interest of brevity we will not report the definition of the Brzozowski derivative here and we refer interested readers to [this article](https://en.wikipedia.org/wiki/Brzozowski_derivative) or to our implementation in `Derivative.elm`.  

We define the relation `r⪼s` as "the expression `r` matches the string `s`". The relation `⪼` is decided as follows:

`r⪼ε ⟺ 	ℇ(r) = ε`

`∀ a ∈ Σ, u ∈ Σ*: `
`r⪼au ⟺ ∂ₐr	⪼ u`

where `∂ₐr` is the derivative of `r` with respect to the symbol `a` and `ℇ(r)` returns `ε` if `r` is nullable and `∅` otherwise.  

## Implementation

We use a standard **tokenize-parse-evaluate** pipeline to convert written regular expressions into ASTs and match them against an input string. In particular, our `Scan.elm` module exposes the function 

`scan : String -> Result E.Error (List T.Token)`

which tries to tokenize the given string. This phase returns an error only when the scanner encounters an invalid escape sequences (e.g. `\` or `\T`). 

Next, the `parse` function in `Parse.elm` tries to construct an AST from a list of tokens: 

`parse : List T.Token -> Result E.Error R.SugarRegExp`

This phase will not allow any malformed expressions from passing through the pipeline by the returning appropriate `SyntaxError`. If successful, `parse` returns a `SugarRegExp` object which is a (semantic) supertype of the `RegExp` type (the definition of which is given above).

Next, we use the `Desugar.elm` module to desugar an `SugarRegExp` expression into a plain `RegExp`. It is this plain object that is used for matching and other downstream tasks. 

The module `Derivative.elm` calculates the Brzozowski derivative of a `RegExp` in a straightforward manner, and `Match.elm` uses this derivative to test string membership. 

## Optimization Strategies

The derivation process, especially for long strings or expressions, produces many superfluously long and verbose intermediate expressions which make the naive algorithm have an exponential-like behavior. For example, in computing the derivative of `a|b` with respect to `a` we get the expression `ε|∅`, which is equivalent to `ε` for matching purposes. Pruning these expressions to their shorter equivalents significantly improves performance. For this reason we define the following function in `RegExp.elm` which tries to fix this problem:

`prune : RegExp -> RegExp`

Another optimization strategy is used in implementing the range sugar expressions (e.g. `a{1,3}`). As we see from their definitions above, these expressions are just the `or` of multiple powers of the expression. This naive implementation is tractable for only small ranges, but not, for example, for `a{1, 10¹⁰}`. A better solution makes use of the fact that 

`∂ₐr{m,n} = (∂ₐr)r{m-1,n-1}`

so we introduce a non-core term in the `RegExp` type called `Repeat` which computes  this derivative internally and runs significantly faster. 

Similarly, character classes, such as `\d = {0,...,9}` are just an `or` over a set of characters. Instead of this naive implementation, we make use of the Unicode (or Ascii) endpoint encodings to decide whether a character class matches a given symbol in constant time. This optimization is manifested in the `member` function in the `Alphabet.elm` module.

## Graphical Interface

In addition to our backend API (the engine), we also provide a user interface to interact with our matcher. The module `UI.elm` is a minimal bridge-point between the engine's Elm API and a JQuery-based HTML website. The latter records the user's input expressions and query strings and sends them over to the engine asking if there is a match. 

The engine checks whether the _entire_ input string is matched, but we are also interested in partial matches. For this reason, the UI scripts dissect the query string into all the possible ordered substrings (`n²-n` substrings) and highlight any matching substrings. 

Our engine also implements an eccentric feature called the `uno` operation which takes a regular expression and returns its semantic reverse. Users can benefit from this feature using the `uno` button placed conveniently next to the input expression textbox. 
