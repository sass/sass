/**
 * All of the deprecation types currently used by Sass.
 *
 * Any of these IDs or the deprecation objects they point to can be passed to
 * `fatalDeprecations`, `futureDeprecations`, or `silenceDeprecations`.
 */
export interface Deprecations {
  // START AUTOGENERATED LIST
  // Checksum: e386251fa5eea522619d91ac98daf84dd474c29d

  /**
   * Deprecation for passing a string directly to meta.call().
   *
   * This deprecation was active in the first version of Dart Sass.
   */
  'call-string': Deprecation<'call-string'>;

  /**
   * Deprecation for @elseif.
   *
   * This deprecation became active in Dart Sass 1.3.2.
   * It became obsolete in Dart Sass 2.0.0.
   */
  elseif: Deprecation<'elseif'>;

  /**
   * Deprecation for @-moz-document.
   *
   * This deprecation became active in Dart Sass 1.7.2.
   * It became obsolete in Dart Sass 2.0.0.
   */
  'moz-document': Deprecation<'moz-document'>;

  /**
   * Deprecation for imports using relative canonical URLs.
   *
   * This deprecation became active in Dart Sass 1.14.2.
   */
  'relative-canonical': Deprecation<'relative-canonical'>;

  /**
   * Deprecation for declaring new variables with !global.
   *
   * This deprecation became active in Dart Sass 1.17.2.
   */
  'new-global': Deprecation<'new-global'>;

  /**
   * Deprecation for using color module functions in place of plain CSS functions.
   *
   * This deprecation became active in Dart Sass 1.23.0.
   */
  'color-module-compat': Deprecation<'color-module-compat'>;

  /**
   * Deprecation for / operator for division.
   *
   * This deprecation became active in Dart Sass 1.33.0.
   */
  'slash-div': Deprecation<'slash-div'>;

  /**
   * Deprecation for leading, trailing, and repeated combinators.
   *
   * This deprecation became active in Dart Sass 1.54.0.
   * It became obsolete in Dart Sass 2.0.0.
   */
  'bogus-combinators': Deprecation<'bogus-combinators'>;

  /**
   * Deprecation for ambiguous + and - operators.
   *
   * This deprecation became active in Dart Sass 1.55.0.
   */
  'strict-unary': Deprecation<'strict-unary'>;

  /**
   * Deprecation for passing invalid units to built-in functions.
   *
   * This deprecation became active in Dart Sass 1.56.0.
   */
  'function-units': Deprecation<'function-units'>;

  /**
   * Deprecation for using !default or !global multiple times for one variable.
   *
   * This deprecation became active in Dart Sass 1.62.0.
   */
  'duplicate-var-flags': Deprecation<'duplicate-var-flags'>;

  /**
   * Deprecation for passing null as alpha in the JS API.
   *
   * This deprecation became active in Dart Sass 1.62.3.
   */
  'null-alpha': Deprecation<'null-alpha'>;

  /**
   * Deprecation for passing percentages to the Sass abs() function.
   *
   * This deprecation became active in Dart Sass 1.65.0.
   */
  'abs-percent': Deprecation<'abs-percent'>;

  /**
   * Deprecation for using the current working directory as an implicit load path.
   *
   * This deprecation became active in Dart Sass 1.73.0.
   */
  'fs-importer-cwd': Deprecation<'fs-importer-cwd'>;

  /**
   * Deprecation for function and mixin names beginning with --.
   *
   * This deprecation became active in Dart Sass 1.76.0.
   */
  'css-function-mixin': Deprecation<'css-function-mixin'>;

  /**
   * Deprecation for declarations after or between nested rules.
   *
   * This deprecation became active in Dart Sass 1.77.7.
   */
  'mixed-decls': Deprecation<'mixed-decls'>;

  /**
   * Deprecation for meta.feature-exists
   *
   * This deprecation became active in Dart Sass 1.78.0.
   */
  'feature-exists': Deprecation<'feature-exists'>;

  /**
   * Deprecation for certain uses of built-in sass:color functions.
   *
   * This deprecation became active in Dart Sass 1.79.0.
   */
  'color-4-api': Deprecation<'color-4-api'>;

  /**
   * Deprecation for using global color functions instead of sass:color.
   *
   * This deprecation became active in Dart Sass 1.79.0.
   */
  'color-functions': Deprecation<'color-functions'>;

  /**
   * Deprecation for legacy JS API.
   *
   * This deprecation became active in Dart Sass 1.79.0.
   */
  'legacy-js-api': Deprecation<'legacy-js-api'>;

  /**
   * Deprecation for @import rules.
   *
   * This deprecation became active in Dart Sass 1.80.0.
   */
  import: Deprecation<'import'>;

  /**
   * Deprecation for global built-in functions that are available in sass: modules.
   *
   * This deprecation became active in Dart Sass 1.80.0.
   */
  'global-builtin': Deprecation<'global-builtin'>;

  /**
   * Deprecation for functions named "type".
   *
   * This deprecation became active in Dart Sass 1.86.0.
   */
  'type-function': Deprecation<'type-function'>;

  // END AUTOGENERATED LIST

  /**
   * Used for any user-emitted deprecation warnings.
   */
  'user-authored': Deprecation<'user-authored', 'user'>;
}

/**
 * Either a deprecation or its ID, either of which can be passed to any of
 * the relevant compiler options.
 *
 * @category Messages
 * @compatibility dart: "1.74.0", node: false
 */
export type DeprecationOrId = Deprecation | keyof Deprecations;

/**
 * The possible statuses that each deprecation can have.
 *
 * "active" deprecations are currently emitting deprecation warnings.
 * "future" deprecations are not yet active, but will be in the future.
 * "obsolete" deprecations were once active, but no longer are.
 *
 * The only "user" deprecation is "user-authored", which is used for deprecation
 * warnings coming from user code.
 *
 * @category Messages
 * @compatibility dart: "1.74.0", node: false
 */
export type DeprecationStatus = 'active' | 'user' | 'future' | 'obsolete';

/**
 * A deprecated feature in the language.
 *
 * @category Messages
 * @compatibility dart: "1.74.0", node: false
 */
export interface Deprecation<
  id extends keyof Deprecations = keyof Deprecations,
  status extends DeprecationStatus = DeprecationStatus
> {
  /** The unique ID of this deprecation. */
  id: id;

  /** The current status of this deprecation. */
  status: status;

  /** A human-readable description of this deprecation. */
  description?: string;

  /** The version this deprecation first became active in. */
  deprecatedIn: status extends 'future' | 'user' ? null : Version;

  /** The version this deprecation became obsolete in. */
  obsoleteIn: status extends 'obsolete' ? Version : null;
}

/**
 * A semantic version of the compiler.
 *
 * @category Messages
 * @compatibility dart: "1.74.0", node: false
 */
export class Version {
  /**
   * Constructs a new version.
   *
   * All components must be non-negative integers.
   *
   * @param major - The major version.
   * @param minor - The minor version.
   * @param patch - The patch version.
   */
  constructor(major: number, minor: number, patch: number);
  readonly major: number;
  readonly minor: number;
  readonly patch: number;

  /**
   * Parses a version from a string.
   *
   * This throws an error if a valid version can't be parsed.
   *
   * @param version - A string in the form "major.minor.patch".
   */
  static parse(version: string): Version;
}

/**
 * An object containing all deprecation types.
 *
 * @category Messages
 * @compatibility dart: "1.74.0", node: false
 */
export const deprecations: Deprecations;
