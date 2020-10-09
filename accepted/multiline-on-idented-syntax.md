# Multiline on idented syntax: Draft 1.0

*([Issues](https://github.com/sass/sass/issues/216),

## Table of Contents

- [Background](#background)
- [Summary](#summary)
- [Syntax](#syntax)

## Background

> This section is non-normative.

When using indented syntax there's no way to split declarations into multiple lines,
making really long declaration less readable or not possible obeying the 80 character limit.

For example, first written in SCSS.

@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0,
           $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, 
           $clear: false, $lead: true, $container: false) {
    color: red;
    display: block;
}

In one line sass,  which is way less readable:

@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0, $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false, $lead: true, $container: false)
    color: red
    display: block

Enabling multiline when using idented syntax would allow declaration similar to that of SCSS.

This proposal provides a way to enable multiline declarations without affecting any backwards compatibility.

Also worth to note that this is a very long awaited feature, issue has been opened since 2011.

## Summary

> This section is non-normative.

When using indented syntax Sass will allow multiline declaration according to parsing context, 
efectively treating multiline input as single line when applicable, example:

```sass
/* Mixins */
@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0,
           $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false,
           $lead: true, $container: false)
    width: $cols
    color: red
    display: block
```
Would be interpreted as:

```sass
@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0, $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false, $lead: true, $container: false)
```

## Syntax

When using idented syntax long declarations can be splitted into multiple lines, 
newline is allowed after opening parenthesis, 
opening square bracket, 
or after a comma, 
in this context newline will be interpreted as non-significant and the next line appened to current line effectively treating input as if it was writen as single line.

Whitespaces are allowed in between the cited characters and the newline, whitespaces after the newline will be interpreted as non-significant as well.

Input:

<x><pre>
@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0,
           $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false,
           $lead: true, $container: false)
</pre></x>

Will be treated as:

<x><pre>
@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0, $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false, $lead: true, $container: false)
</pre></x>
