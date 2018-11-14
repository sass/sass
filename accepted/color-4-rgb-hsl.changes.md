## Draft 1.3

* Refactor `rgb()` and `hsl()` to reduce repetition and ensure that they always
  return plain CSS function calls with the correct names.

* Fix a redirect that didn't get the extra text about renaming its plain CSS.

## Draft 1.2

* Support `rgb(var(--foo) / 0.5)` and `hsl(var(--foo) / 0.5)`, since the
  variable could expand to a list of arguments.

* Remove the `hsl($color, $alpha)` overload, since it redirects to an `hsla()`
  overload that doesn't exist.

* Ensure that function redirects always return plain CSS functions with the same
  name as that written by the user.

## Draft 1.1

* Specify behavior for special variable functions, which may expand into
  multiple arguments.

* Specify behavior for special number functions which are converted into strings
  due to `/`.

## Draft 1

* Initial draft.
