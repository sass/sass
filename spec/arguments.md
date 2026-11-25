# Arguments and Parameters

## Table of Contents

* [Syntax](#syntax)
  * [`ArgumentList`](#argumentlist)
  * [`ParameterList`](#parameterlist)
* [Semantics](#semantics)

## Syntax

### `ArgumentList`

<x><pre>
**ArgumentList**¹ ::= '(' Argument (',' Argument)\* ','? ')'
&#32;               | '(' ')'
**Argument**      ::= Expression | NamedArgument | RestArgument
**NamedArgument** ::= [PlainVariable] ':' Expression
**RestArgument**  ::= Expression '...'
</pre></x>

1: `Expression`s must precede `NamedArguments` which must precede
`RestArgument`s. An `ArgumentList` may have at most two `RestArgument`s. No two
`NamedArgument`s may have the same name.

### `ParameterList`

<x><pre>
**ParameterList**¹  ::= '(' Parameter (',' Parameter)\* ','? ')'
&#32;                 | '(' ')'
**Parameter**       ::= SingleParamter | RestParameter
**SingleParameter** ::= [PlainVariable] (':' Expression)?
**RestParameter**   ::= [PlainVariable] '...'
</pre></x>

[PlainVariable]: variables.md#syntax

1: `SingleParameter`s must precede `RestParameter`s. A `ParameterList` may have
at most one `RestParameter`. No two parameters may have the same name.

## Semantics

To execute an `ArgumentList` `args` with a `ParameterList` `params`:

* Let `positional` be an empty list and `named` be an empty map.

* For each argument `arg` in `args`:

  * If `arg` is an `Expression`, evaluate it in the scope of the caller and add
    the resulting value to `positional`.

  * If `arg` is a `NamedArgument`, evaluate its `Expression` in the scope of
    caller, then add it as a value to `named` with the `PlainVariable`'s name as
    its key.

  * If `arg` is a `RestArgument`:

    * Let `arg-value` be the result of evaluating `arg`'s expression.

    * If `arg-value` is a map:

      * For each key/value pair `name`/`value` in `arg-value`:

        * If `name` isn't a string, throw an error.

        * Add `name`'s [semantic value] as a key to `named` with `value` as
          its value. If there's already a key named `named`, throw an error.

    * Otherwise, if `arg` is the second `RestArgument` in `args`,
      throw an error.

    * Otherwise:

      * Add each element of `arg-value`'s [list value] to `positional`.

      * If `arg-value` is an argument list, add its named arguments to `named`.
        Throw an error if any names already exist in `named`.

* For each `SingleParameter` `param` in `params`:

  * Let `name` be `param`'s `PlainVariable`'s name.

  * If `name` exists in `named`:

    * Define a variable named `name` in the current scope whose value is
      `named[name]`.

    * Remove `name` and its value from `named`.

  * Otherwise, if `positional` isn't empty:

    * Define a variable named `name` in the current scope whose value is the
      first element of `positional`.

    * Remove the first element from `positional`.

  * Otherwise, if `param` has an `Expression`, define a variable named `name` in
    the current scope whose value is the result of evaluating that `Expression`
    in the current scope.

  * Otherwise, throw an error.

* If `params` has a `RestParameter`:

  * Let `name` be that `RestParameter`'s `PlainVariable`'s name.

  * Let `arglist` be an argument list whose positional arguments are
    `positional` and whose named arguments are `named`.

  * Define a variable named `name` in the current scope whoes value is
    `arglist`.

* Otherwise, if `positional` or `named` aren't empty, throw an error.

[semantic value]: types/string.md#getting-the-semantic-value-of-a-string
[list value]: types/argument-list.md#types
