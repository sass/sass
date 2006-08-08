

module HAMLHelpers
  def flatten(input)
    input.gsub(/\n/, '&#x000A;').gsub(/\r/, '')
  end

  def tupleize(first, second)
    second = second.reverse
    first.collect do |f|
      [f, second.pop]
    end
  end

  def find_and_flatten(input)
    s, result = StringScanner.new(input), ""
    while s.scan_until(/<(textarea|code|pre)[^>]*>/i)
      result += s.pre_match.to_s + s.matched
      tag_name = s.matched.scan(/(textarea|code|pre)/).first
      result += flatten(s.scan_until(/<\/#{tag_name}>/i))
    end
    result + s.rest
  end
end
