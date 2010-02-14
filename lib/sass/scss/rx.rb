module Sass
  module SCSS
    # A module containing regular expressions used
    # for lexing tokens in an SCSS document.
    # Most of these are taken from [the CSS3 spec](http://www.w3.org/TR/css3-syntax/#lexical),
    # although some have been modified for various reasons.
    module RX
      # Creates a Regexp from a plain text string,
      # escaping all significant characters.
      #
      # @param str [String] The text of the regexp
      # @param flags [Fixnum] Flags for the created regular expression
      # @return [Regexp]
      # @private
      def self.quote(str, flags = 0)
        Regexp.new(Regexp.quote(str), flags)
      end

      H        = /[0-9a-f]/i
      NL       = /\n|\r\n|\r|\f/
      NONASCII = /[\200-\377]/
      UNICODE  = /\\#{H}{1,6}[ \t\r\n\f]?/
      ESCAPE   = /#{UNICODE}|\\[ -~\200-\377]/
      NMSTART  = /[a-z]|#{NONASCII}|#{ESCAPE}/i
      NMCHAR   = /[a-z0-9_-]|#{NONASCII}|#{ESCAPE}/i
      STRING1  = /\"((?:[^\n\r\f\\"]|\\#{NL}|#{ESCAPE})*)\"/
      STRING2  = /\'((?:[^\n\r\f\\']|\\#{NL}|#{ESCAPE})*)\'/

      IDENT    = /[-_]?#{NMSTART}#{NMCHAR}*/
      NAME     = /#{NMCHAR}+/
      NUM      = /[0-9]+|[0-9]*.[0-9]+/
      STRING   = /#{STRING1}|#{STRING2}/
      URL      = /((?:[!#%$&*-~]|#{NONASCII}|#{ESCAPE})*)/
      W        = /[ \t\r\n\f]*/

      # This is more liberal than the spec's definition,
      # but that definition didn't work well with the greediness rules
      RANGE    = /(?:#{H}|\?){1,6}/

      ##

      S = /[ \t\r\n\f]+/

      COMMENT = /\/\*[^*]*\*+(?:[^\/][^*]*\*+)*\//
      SINGLE_LINE_COMMENT = /\/\/.*/

      CDO            = quote("<!--")
      CDC            = quote("-->")
      INCLUDES       = quote("~=")
      DASHMATCH      = quote("|=")
      PREFIXMATCH    = quote("^=")
      SUFFIXMATCH    = quote("$=")
      SUBSTRINGMATCH = quote("*=")

      HASH = /##{NAME}/

      IMPORTANT = /!#{W}important/i

      NUMBER = /#{NUM}(?:#{IDENT}|%)?/

      URI = /url\(#{W}(?:#{STRING}|#{URL})#{W}\)/i
      FUNCTION = /#{IDENT}\(/

      UNICODERANGE = /u\+(?:#{H}{1,6}-#{H}{1,6}|#{RANGE})/i

      # Defined in http://www.w3.org/TR/css3-selectors/#lex
      PLUS = /#{W}\+/
      GREATER = /#{W}>/
      TILDE = /#{W}~/
      NOT = quote(":not(", Regexp::IGNORECASE)

      # Custom
      HEXCOLOR = /\#[0-9a-fA-F]{3}(?:[0-9a-fA-F]{3})?/
    end
  end
end
