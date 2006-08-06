

module HAMLHelpers
  def flatten(input)
    input.gsub(/\n/, '&#x000A').gsub(/\r/, '')
  end

  def tupleize(first, second)
    second = second.reverse
    first.collect do |f|
      [f, second.pop]
    end
  end
end
