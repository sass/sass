/**
 * # JavaScript Deprecations API: Draft 1
 *
 * *([Issue](https://github.com/sass/sass/issues/3520))*
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
 * This proposal adds two new interfaces to the JS API (`Deprecation` and
 * `Version`), three new optional properties on `Options` (`fatalDeprecations`,
 * `silenceDeprecations`, and `futureDeprecations`), a new parameter on
 * `Logger.warn` (`options.deprecationType`) two type aliases (`DeprecationOrId`
 * and `DeprecationStatus`) and a new object `deprecations` that contains the
 * various `Deprecation` objects.
 *
 * All language-wide deprecations are specified in `deprecations`, and any new
 * deprecations added in the future should update the specification accordingly.
 * However, the `deprecations` object also allows individual implementations to
 * add their own, implementation-specific deprecations.
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
 * Additionally, while a deprecation's status is part of the specification, we
 * chose to leave the `deprecatedIn` and `obsoleteIn` versions of each
 * deprecation out of the specification. As the two current implementers of this
 * API are both based on Dart Sass, these versions are _currently_ consistent
 * across implementations in practice, potential future implementers should not
 * need to be tied to Dart Sass's versioning.
 *
 * #### Errors and Warnings for Invalid Deprecations
 *
 * For all of the deprecation options, we chose to have the API throw an error
 * if a string ID is passed for a deprecation that does not exist. This will
 * ensure that typos are caught early and is consistent with the behavior on the
 * command line.
 *
 * For `fatalDeprecations`, we chose to make passing in a future deprecation
 * that's not also passed to `futureDeprecations` an error, since, in a
 * sense, future deprecations aren't really deprecations yet unless they're
 * enabled via `futureDeprecations`. This also prevents someone who just passes
 * all deprecations to `fatalDeprecations` from accidentally opting into a
 * deprecation that hasn't been released yet. This is not yet consistent with
 * the behavior on the command line / Dart API, but we plan to update them to
 * match this specification.
 *
 * For obsolete deprecations passed to `fatalDeprecations`, we chose to emit a
 * warning so users can know to update their configuration, but we allow
 * compilation to proceed without an error, since an obsolete deprecation means
 * the underlying behavior has now been removed, which should match the behavior
 * of making that deprecation fatal while it was still active.
 *
 * For `silenceDeprecations`, we chose to make passing anything other than
 * an active deprecation an error to prevent users from unintentionally
 * over-silencing. Future deprecations are already effectively silenced, so
 * users shouldn't be able to pass it here until it's actually active. If the
 * deprecation a user was silencing is now obsolete, that means the breaking
 * change has now been made, so silencing has no effect, and users who were
 * silencing it may not be ready for the actual breaking change. There is no
 * way to distinguish between different user-authored deprecations, so they
 * should not be able to be silenced as a group.
 *
 * For `futureDeprecations`, we only emit a warning for non-future deprecations,
 * since future deprecations all eventually become active. The warning will let
 * users know to clean up their configuration, but there should be no behavior
 * change.
 */

/* ## API */

import {SourceSpan} from '../spec/js-api';

declare module '../spec/js-api' {
  interface Options<sync extends 'sync' | 'async'> {
    /**
     * A set of deprecations to treat as fatal.
     *
     * If a version is provided, then all deprecations released in that
     * version or earlier will be treated as fatal. If a string is provided, it
     * will resolve to the deprecation with that ID, or error if none exists.
     *
     * This will throw an error if a future deprecation is included here, unless
     * that future deprecation is also passed to `futureDeprecations`. This
     * will emit a warning if an obsolete deprecation is included here. */
    fatalDeprecations?: (Deprecation | string | Version)[];

    /**
     * A set of active deprecations to ignore.
     *
     * If a string is provided, it will resolve to the deprecation with that ID,
     * or error if none exists.
     *
     * This will throw an error if a future, obsolete, or user-authored
     * deprecation is included here.
     */
    silenceDeprecations?: (Deprecation | string)[];

    /**
     * A set of future deprecations to opt into early.
     *
     * If a string is provided, it will resolve to the deprecation with that ID,
     * or error if none exists.
     *
     * This will emit a warning if a non-future deprecation is included here.
     */
    futureDeprecations?: (Deprecation | string)[];
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
   * An object containing all of the deprecations recognized by this
   * implementation as properties, as well as a `find` function to get the
   * deprecation for a given ID.
   */
  export const deprecations: Readonly<
    _Deprecations & {[key: string]: Deprecation}
  >;
}

interface _Deprecations {
  /** Deprecation for passing a string to `call` instead of `get-function`. */
  callString: Deprecation<'call-string', 'active'>;

  /** Deprecation for `@elseif`. */
  elseif: Deprecation<'elseif', 'active'>;

  /** Deprecation for parsing `@-moz-document`. */
  mozDocument: Deprecation<'moz-document', 'active'>;

  /** Deprecation for importers using relative canonical URLs. */
  relativeCanonical: Deprecation<'relative-canonical', 'active'>;

  /** Deprecation for declaring new variables with `!global`. */
  newGlobal: Deprecation<'new-global', 'active'>;

  /**
   * Deprecation for certain functions in the color module matching the
   * behavior of their global counterparts for compatibility reasons.
   */
  colorModuleCompat: Deprecation<'color-module-compat', 'active'>;

  /** Deprecation for treaing `/` as division. */
  slashDiv: Deprecation<'slash-div', 'active'>;

  /** Deprecation for leading, trailing, and repeated combinators. */
  bogusCombinators: Deprecation<'bogus-combinators', 'active'>;

  /** Deprecation for ambiguous `+` and `-` operators. */
  strictUnary: Deprecation<'strict-unary', 'active'>;

  /** Deprecation for passing invalid units to certain built-in functions. */
  functionUnits: Deprecation<'function-units', 'active'>;

  /** Deprecation for `@import` rules. */
  importRules: Deprecation<'import', 'future'>;

  /** Used for deprecations coming from user-authored code. */
  userAuthored: Deprecation<'user-authored', 'user'>;

  /**
   * Returns the deprecation with the given ID, or null if none exists.
   *
   * This must support all specified deprecations, but implementations may also
   * have their own deprecations beyond those specified by the language.
   */
  find(id: string): Deprecation<typeof id, DeprecationStatus> | null;
}

/** A deprecation, or the ID of one. */
export type DeprecationOrId = Deprecation | string;

/** A deprecation's status. */
export type DeprecationStatus = 'active' | 'user' | 'future' | 'obsolete';

/** A deprecated feature in the language. */
export interface Deprecation<
  id extends string = string,
  status extends DeprecationStatus = DeprecationStatus
> {
  /**
   * An ID for this deprecation.
   *
   * For language-wide deprecations specified in the API, this will be a short,
   * alphanumeric, kebab-case identifier. Implementation-specific deprecations
   * should be prefixed with an implementation identifier followed by a slash to
   * avoid collision with future language-wide deprecations.
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
export interface Version {
  /**
   * The major version.
   *
   * This must be a non-negative integer.
   */
  major: number;

  /**
   * The minor version.
   *
   * This must be a non-negative integer.
   */
  minor: number;

  /**
   * The patch version.
   *
   * This must be a non-negative integer.
   */
  patch: number;
}
