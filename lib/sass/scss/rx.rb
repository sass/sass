module Sass
  module SCSS
    # A module containing regular expressions used
    # for lexing tokens in an SCSS document.
    # Most of these are taken from [the CSS3 spec](http://www.w3.org/TR/css3-syntax/#lexical),
    # although some have been modified for various reasons.
    module RX
      # Takes a string and returns a CSS identifier
      # that will have the value of the given string.
      #
      # @param str [String] The string to escape
      # @return [String] The escaped string
      def self.escape_ident(str)
        return "" if str.empty?
        return "\\#{str}" if str == '-' || str == '_'
        out = ""
        value = str.dup
        out << value.slice!(0...1) if value =~ /^[-_]/
        if value[0...1] =~ NMSTART
          out << value.slice!(0...1)
        else
          out << escape_char(value.slice!(0...1))
        end
        out << value.gsub(/[^a-zA-Z0-9_-]/) {|c| escape_char c}
        return out
      end

      # Escapes a single character for a CSS identifier.
      #
      # @param c [String] The character to escape. Should have length 1
      # @return [String] The escaped character
      # @private
      def self.escape_char(c)
        return "\\%06x" % Sass::Util.ord(c) unless c =~ /[ -\/:-~]/
        return "\\#{c}"
      end

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

      H        = /[0-9a-fA-F]/
      NL       = /\n|\r\n|\r|\f/
      UNICODE  = /\\#{H}{1,6}[ \t\r\n\f]?/
      s = if Sass::Util.ruby1_8?
            '\200-\377'
          else
            '\u{80}-\u{D7FF}\u{E000}-\u{FFFD}\u{10000}-\u{10FFFF}'
          end
      NONASCII = /[#{s}]/
      ESCAPE   = /#{UNICODE}|\\[ -~#{s}]/
      NMSTART  = /[_a-zA-Z]|#{NONASCII}|#{ESCAPE}/
      NMCHAR   = /[a-zA-Z0-9_-]|#{NONASCII}|#{ESCAPE}/
      STRING1  = /\"((?:[^\n\r\f\\"]|\\#{NL}|#{ESCAPE})*)\"/
      STRING2  = /\'((?:[^\n\r\f\\']|\\#{NL}|#{ESCAPE})*)\'/

      IDENT    = /-?#{NMSTART}#{NMCHAR}*/
      NAME     = /#{NMCHAR}+/
      NUM      = /[0-9]+|[0-9]*\.[0-9]+/
      STRING   = /#{STRING1}|#{STRING2}/
      URLCHAR  = /[#%&*-~]|#{NONASCII}|#{ESCAPE}/
      URL      = /(#{URLCHAR}*)/
      W        = /[ \t\r\n\f]*/
      VARIABLE = /(\$)(#{Sass::SCSS::RX::IDENT})/

      # This is more liberal than the spec's definition,
      # but that definition didn't work well with the greediness rules
      RANGE    = /(?:#{H}|\?){1,6}/

      ##

      S = /[ \t\r\n\f]+/

      COMMENT = /\/\*[^*]*\*+(?:[^\/][^*]*\*+)*\//
      SINGLE_LINE_COMMENT = /\/\/.*(\n[ \t]*\/\/.*)*/

      CDO            = quote("<!--")
      CDC            = quote("-->")
      INCLUDES       = quote("~=")
      DASHMATCH      = quote("|=")
      PREFIXMATCH    = quote("^=")
      SUFFIXMATCH    = quote("$=")
      SUBSTRINGMATCH = quote("*=")

      HASH = /##{NAME}/

      IMPORTANT = /!#{W}important/i
      DEFAULT = /!#{W}default/i

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
      HEXCOLOR = /\#[0-9a-fA-F]+/
      INTERP_START = /#\{/
      MOZ_ANY = quote(":-moz-any(", Regexp::IGNORECASE)

      IDENT_HYPHEN_INTERP = /-(#\{)/
      STRING1_NOINTERP = /\"((?:[^\n\r\f\\"#]|#(?!\{)|\\#{NL}|#{ESCAPE})*)\"/
      STRING2_NOINTERP = /\'((?:[^\n\r\f\\'#]|#(?!\{)|\\#{NL}|#{ESCAPE})*)\'/
      STRING_NOINTERP = /#{STRING1_NOINTERP}|#{STRING2_NOINTERP}/
      # Can't use IDENT here, because it seems to take exponential time on 1.8.
      # We could use it for 1.9 only, but I don't want to introduce a cross-version
      # behavior difference.
      # In any case, almost all CSS idents will be matched by this.
      #
      # We explicitly avoid parsing newlines or values/selectors longer than
      # about 50 characters. This mitigates the problem of exponential parsing
      # time when a value has a long string of valid, parsable content followed
      # by something invalid.
      STATIC_VALUE = /(-?#{NMSTART}|#{STRING_NOINTERP}|[ \t](?!%)|#[a-f0-9]|[,%]|#{NUM}|\!important){0,50}([;}])/i
      STATIC_SELECTOR = /(#{NMCHAR}|[ \t]|[,>+*]|[:#.]#{NMSTART}){0,50}([{])/i
    end
  end
end
