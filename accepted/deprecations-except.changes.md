## 2.0

* Dropped the `Record` supertype from the `Deprecations` interface. This
  supertype changed `keyof Deprecations` (used in the `DeprecationOrId` type)
  from an explicit disjunction of known keys to `string | number`, which is not
  only substantially less useful as a static check but a potentially breaking
  change for downstream TypeScript consumers.

* Renamed `Options.silentDeprecations` to `Options.silenceDeprecations` to match
  the existing field.

* Renamed `DeprecationSelectorObject` to `DeprecationSelectorDifference` to more
  clearly indicate its behavior. This allows us to add different object types
  later to represent different operations, if we want to.

## 1.0

* Initial draft.
