module Sass
  module SCSS
    module RX
      def self.quote(str, flags = 0)
        Regexp.new(Regexp.quote(str), flags)
      end

      H        = /[0-9a-f]/i
      NL       = /\n|\r\n|\r|\f/
      NONASCII = /[\200-\377]/
      UNICODE  = /\\#{H}{1,6}[ \t\r\n\f]?/
      ESCAPE   = /#{UNICODE}|\\[ -~\200-\377]/
      NMSTART  = /[a-z]|#{NONASCII}|#{ESCAPE}/i
      NMCHAR   = /[a-z0-9-]|#{NONASCII}|#{ESCAPE}/i
      STRING1  = /\"([\t !#%$&(-~]|\\#{NL}|\'|#{NONASCII}|#{ESCAPE})*\"/
      STRING2  = /\'([\t !#%$&(-~]|\\#{NL}|\"|#{NONASCII}|#{ESCAPE})*\'/

      IDENT    = /[-]?#{NMSTART}#{NMCHAR}*/
      NAME     = /#{NMCHAR}+/
      NUM      = /[0-9]+|[0-9]*.[0-9]+/
      STRING   = /#{STRING1}|#{STRING2}/
      URL      = /([!#%$&*-~]|#{NONASCII}|#{ESCAPE})*/
      W        = /[ \t\r\n\f]*/
      RANGE    = /\?{1,6}|#{H}(\?{0,5}|#{H}(\?{0,4}|#{H}(\?{0,3}|#{H}(\?{0,2}|#{H}(\??|#{H})))))/

      ##

      S = /[ \t\r\n\f]+/

      COMMENT = /\/\*[^*]*\*+([^\/][^*]*\*+)*\//

      CDO            = quote("<!--")
      CDC            = quote("-->")
      INCLUDES       = quote("~=")
      DASHMATCH      = quote("|=")
      PREFIXMATCH    = quote("^=")
      SUFFIXMATCH    = quote("$=")
      SUBSTRINGMATCH = quote("*=")

      HASH = /##{NAME}/

      IMPORT    = quote("@import", Regexp::IGNORECASE)
      PAGE      = quote("@page", Regexp::IGNORECASE)
      MEDIA     = quote("@media", Regexp::IGNORECASE)
      FONT_FACE = quote("@font-face", Regexp::IGNORECASE)
      CHARSET   = quote("@charset", Regexp::IGNORECASE)
      NAMESPACE = quote("@namespace", Regexp::IGNORECASE)

      IMPORTANT = /!#{W}important/i

      NUMBER = /#{NUM}(#{IDENT}|%)?/

      URI = /url\(#{W}(#{STRING}|#{URL})#{W}\)/i
      FUNCTION = /#{IDENT}\(/

      UNICODERANGE = /u\+(#{RANGE}|#{H}{1,6}-#{H}{1,6})/
    end
  end
end
