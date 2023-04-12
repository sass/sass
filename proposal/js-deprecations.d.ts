/**
 * # JavaScript Deprecations API: Draft 1.1
 *
 * *([Issue](https://github.com/sass/sass/issues/3520),
 * [Changelog](js-deprecations.changes.md))*
 *
 * ## Background
 *
 * > This section is non-normative.
 *
 * We recently added support to Dart Sass that allowed users to opt in to
 * treating deprecation warnings as errors (on a per-deprecation basis), as
 * well as opting in early to certain future deprecations. This is currently
 * supported on the command line and via the Dart API, but we'd like to extend
 * this support to the JS API as well.
 *
 * We would also like to add support for silencing a particular deprecation's
 * warnings, primarily to enable a gentler process for deprecating `@import`.
 *
 * ## Summary
 *
 * > This section is non-normative.
 *
 * This proposal adds a new `Deprecation` interface and `Version` class to the
 * JS API, three new optional properties on `Options` (`fatalDeprecations`,
 * `silenceDeprecations`, and `futureDeprecations`), a new parameter on
 * `Logger.warn` (`options.deprecationType`) two type aliases (`DeprecationOrId`
 * and `DeprecationStatus`) and a new object `deprecations` that contains the
 * various `Deprecation` objects.
 *
 * All deprecations are specified in `deprecations`, and any new deprecations
 * added in the future (even those specific to a particular implementation)
 * should update the specification accordingly. Deprecations should never be
 * removed from the specification; when the behavior being deprecated is removed
 * (i.e. there's a major version release), the deprecation status should be
 * changed to obsolete, but remain in the specification.
 *
 * Every `Deprecation` has a unique `id`, one of four `status` values, and
 * (optionally) a human-readable `description`. Depending on the status, each
 * deprecation may also have a `deprecatedIn` version and an `obsoleteIn`
 * version that specify the compiler versions the deprecation became active
 * and became obsolete in, respectively.
 *
 * ### Design Decisions
 *
 * #### Exposing the Full `Deprecation` Interface
 *
 * One alternative to specifying a full `Deprecation` interface is to just have
 * the relevant APIs take in string IDs. We considered this, but concluded that
 * each deprecation has additional metadata that users of the API may wish to
 * access (for example, a bundler may wish to surface the `description` and
 * `deprecatedIn` version to its users).
 *
 * #### Formally Specifying the Deprecations
 *
 * We chose to make the list of deprecations part of the specification itself,
 * as this ensures that the language-wide deprecations are consistent across
 * implementations. However, if an implementation wishes to add a deprecation
 * that applies only to itself, it may still do so.
 *
 * Additionally, we chose to leave the `status`, `deprecatedIn` version, and
 * `obsoleteIn` version of each deprecation out of the specification. As the
 * two current implementers of this API are both based on Dart Sass, these
 * fields are _currently_ consistent across implementations in practice, but
 * the status of each deprecation in other potential future implementers would
 * not always match Dart Sass, and the relevent versions would almost certainly
 * not match.
 *
 * #### Warnings for Invalid Deprecations and Precedence of Options
 *
 * Whenever potentially invalid sets of deprecations are passed to any of the
 * options, we choose to emit warnings rather than errors, as the status of
 * each deprecation can change over time, and users may share a configuration
 * when compiling across multiple implementations/versions whose dependency
 * statuses may not be in sync.
 *
 * The situations we chose to warn for are:
 *
 * - an invalid string ID.
 *
 *   This is disallowed by the API's types, but may still occur at runtime,
 *   and should be warned for accordingly.
 *
 * - a future deprecation is passed to `fatalDeprecations` but not
 *   `futureDeprecations`.
 *
 *   In this scenario, the future deprecation will still be treated as fatal,
 *   but we want to warn users to prevent situtations where a user tries to
 *   make every deprecation fatal and ends up including future ones too.
 *
 * - an obsolete deprecation is passed to `fatalDeprecations`.
 *
 *   If a deprecation is obsolete, that means the breaking change has already
 *   happened, so making it fatal is a no-op.
 *
 * - passing anything other than an active deprecation to `silenceDeprecations`.
 *
 *   This is particularly important for obsolete deprecations, since otherwise
 *   users may not be aware of a subtle breaking change for which they were
 *   previously silencing warnings. We also warn for passing
 *   `Deprecation.userAuthored`, since there's no way to distinguish between
 *   different deprecations from user-authored code, so silencing them as a
 *   group is inadvisable. Passing a future deprecation here is either a no-op,
 *   or cancels out passing it to `futureDeprecations`, so we warn for that as
 *   well.
 *
 * - passing a non-future deprecation to `futureDeprecations`.
 *
 *   This is a no-op, so we should warn users so they can clean up their
 *   configuration.
 */

/* ## API */

import {SourceSpan} from '../spec/js-api';

declare module '../spec/js-api' {
  interface Options<sync extends 'sync' | 'async'> {
    /**
     * A set of deprecations to treat as fatal.
     *
     * If a deprecation warning of any provided type is encountered during
     * compilation, the compiler must error instead.
     *
     * The compiler should convert any string passed here to a `Deprecation`
     * by indexing `deprecations`.     *
     * If a version is passed here, it should be treated equivalently to passing
     * all active deprecations whose `deprecatedIn` version is less than or
     * equal to it.
     *
     * The compiler must error if a future deprecation is included here, unless
     * that future deprecation is also passed to `futureDeprecations`. It must
     * emit a warning if an obsolete deprecation is included here.
     *
     * If a deprecation is passed both here and to `silenceDeprecations`, a
     * warning must be emitted, but making the deprecation fatal must take
     * precedence.
     */
    fatalDeprecations?: (DeprecationOrId | Version)[];

    /**
     * A set of active deprecations to ignore.
     *
     * If a deprecation warning of any provided type is encountered during
     * compilation, the compiler must ignore it.
     *
     * The compiler should convert any string passed here to a `Deprecation`
     * by indexing `Deprecations`.
     *
     * The compiler must error if an obsolete deprecation or
     * `deprecations['user-authored']` is included here. It must emit a warning
     * if a future deprecation is included here, but silencing it takes
     * precedence over `futureDeprecations` enabling it.
     */
    silenceDeprecations?: DeprecationOrId[];

    /**
     * A set of future deprecations to opt into early.
     *
     * For each future deprecation provided here, the compiler must treat that
     * deprecation as if it is active, emitting warnings as necessary (subject
     * to `fatalDeprecations` and `silenceDeprecations`).
     *
     * The compiler should convert any string passed here to a `Deprecation`
     * by indexing `Deprecations`.
     *
     * The compiler must emit a warning if a non-future deprecation is included
     * here.
     */
    futureDeprecations?: DeprecationOrId[];
  }

  interface Logger {
    /**
     * Update the third sub-bullet of bullet two to read:
     *
     * If this warning is caused by behavior that used to be allowed but will
     * be disallowed in the future, set `options.deprecation` to `true` and
     * set `options.deprecationType` to the relevant `Deprecation`. Otherwise,
     * set `options.deprecation` to `false` and leave `options.deprecationType`
     * undefined.
     */
    warn?(
      message: string,
      options: {
        deprecation: boolean;
        deprecationType?: Deprecation;
        span?: SourceSpan;
        stack?: string;
      }
    ): void;
  }

  /**
   * An object containing all of the deprecations.
   */
  export const deprecations: Deprecations;
}

interface Deprecations {
  /** Deprecation for passing a string to `call` instead of `get-function`. */
  'call-string': Deprecation<'call-string'>;

  /** Deprecation for `@elseif`. */
  elseif: Deprecation<'elseif'>;

  /** Deprecation for parsing `@-moz-document`. */
  'moz-document': Deprecation<'moz-document'>;

  /** Deprecation for importers using relative canonical URLs. */
  'relative-canonical': Deprecation<'relative-canonical'>;

  /** Deprecation for declaring new variables with `!global`. */
  'new-global': Deprecation<'new-global'>;

  /**
   * Deprecation for certain functions in the color module matching the
   * behavior of their global counterparts for compatibility reasons.
   */
  'color-module-compat': Deprecation<'color-module-compat'>;

  /**
   * Deprecation for treaing `/` as division.
   *
   * Update the proposal for forward slash as a separator to say that it emits
   * deprecation warnings with ID 'slash-div'.
   */
  'slash-div': Deprecation<'slash-div'>;

  /**
   * Deprecation for leading, trailing, and repeated combinators.
   *
   * Update the proposal for bogus combinators to say that it emits deprecation
   * warnings with ID 'bogus-combinators'.
   */
  'bogus-combinators': Deprecation<'bogus-combinators'>;

  /**
   * Deprecation for ambiguous `+` and `-` operators.
   *
   * Update the proposal for strict unary operators to say that it emits
   * deprecation warnings with ID 'strict-unary'.
   */
  'strict-unary': Deprecation<'strict-unary'>;

  /**
   * Deprecation for passing invalid units to certain built-in functions.
   *
   * Update the proposals for function units, random with units, and angle units
   * to say that they emit deprecation warnings with ID 'function-units'.
   */
  'function-units': Deprecation<'function-units'>;

  /**
   * Deprecation for using multiple `!global` or `!default` flags on a single
   * variable.
   *
   * > This deprecation was never explicitly listed in a proposal.
   */
  'duplicate-var-flags': Deprecation<'duplicate-var-flags'>;

  /**
   * Deprecation for `@import` rules.
   *
   * Update the proposal for the module system to say that, when `@import` is
   * deprecated, Sass will emit deprecation warnings with ID 'import' when
   * `@import` rules are encountered.
   */
  import: Deprecation<'import'>;

  /** Used for deprecations coming from user-authored code. */
  'user-authored': Deprecation<'user-authored', 'user'>;
}

/** A deprecation, or the ID of one. */
export type DeprecationOrId = Deprecation | keyof Deprecations;

/** A deprecation's status. */
export type DeprecationStatus = 'active' | 'user' | 'future' | 'obsolete';

/** A deprecated feature in the language. */
export interface Deprecation<
  id extends keyof Deprecations = keyof Deprecations,
  status extends DeprecationStatus = DeprecationStatus
> {
  /**
   * A kebab-case ID for this deprecation.
   */
  id: id;

  /**
   * The status of this deprecation.
   *
   * - 'active' means this deprecation is currently enabled. `deprecatedIn` is
   *   non-null and `obsoleteIn` is null.
   * - 'user' means this deprecation is from user-authored code. Both
   *   `deprecatedIn` and `obsoleteIn` are null.
   * - 'future' means this deprecation is not yet enabled. Both `deprecatedIn`
   *   and `obsoleteIn` are null.
   * - 'obsolete' means this deprecation is now obsolete, as the feature it was
   *   for has been fully removed. Both `deprecatedIn` and `obsoleteIn` are
   *   non-null.
   */
  status: status;

  /** A brief user-readable description of this deprecation. */
  description?: string;

  /**
   * The compiler version this feature was first deprecated in.
   *
   * This is implementation-dependent, so versions are not guaranteed to be
   * consistent between different compilers. For future deprecations, or those
   * originating from user-authored code, this is null.
   */
  deprecatedIn: status extends 'future' | 'user' ? null : Version;

  /**
   * The compiler version this feature was fully removed in, making the
   * deprecation obsolete.
   *
   * This is null for active and future deprecations.
   */
  obsoleteIn: status extends 'obsolete' ? Version : null;
}

/** A semantic version of the compiler. */
export class Version {
  /**
   * The major version.
   *
   * This must be a non-negative integer.
   */
  readonly major: number;

  /**
   * The minor version.
   *
   * This must be a non-negative integer.
   */
  readonly minor: number;

  /**
   * The patch version.
   *
   * This must be a non-negative integer.
   */
  readonly patch: number;

  constructor(major: number, minor: number, patch: number);

  /**
   * Parses a string in the form "major.minor.patch" into a `Version`.
   */
  static parse(version: string): Version;
}
