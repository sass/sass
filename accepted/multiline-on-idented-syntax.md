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

## Summary

> This section is non-normative.

Sass will allow multiline declaration by using backslash "\" to wrap long lines, 
treating it as single line, 
so should only be used where single line is used now, example:

```sass
/* Mixins */
@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0, \
           $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false, \
           $lead: true, $container: false)
    width: $cols
    color: red
    display: block
```

```sass
/* Font faces */
@font-face
    font-family: 'SPEdessa'
    src: url('fonts/spedessa-webfont.eot')
    src: url('fonts/spedessa-webfont.eot?#iefix') format('embedded-opentype'), \
        url('fonts/spedessa-webfont.woff') format('woff'), \
        url('fonts/spedessa-webfont.ttf') format('truetype')
    font-weight: normal
    font-style: normal
```

```sass
/* List */
$list:  (1,  "value"), \
    (5,  "value"), \
    (23, "value"), \
    (85, "value"), \
    (32, "value"), \
    (11, "value"), \
    (35, "value"), \
    (89, "value")
```

```sass
/* Maps */
$susy: ( columns: 12, \
  gutters: 1/3, \
  gutter-position: after, \
  math: fluid, \
  output: float, \
  flow: ltr, \
  global-box-sizing: border-box )
```


## Syntax

Backslash most be followed by newline, otherwise will throw an error,
backslash will be ignored as well as spaces after,
making the input similar to being writen in a single line.

Input:

<x><pre>
@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0, \
           $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false, \
           $lead: true, $container: false)
</pre></x>

Will be treated as:

<x><pre>
@mixin col($cols, $mleft: 0, $mright: 0, $include-margin: false, $border: 0, $pleft: 0, $pright: 0, $include-padding: true, $extra: 0, $clear: false, $lead: true, $container: false)
</pre></x>
