

module HAMLHelpers
  def flatten(input)
    input.gsub(/\n/, '&#x000A').gsub(/\r/, '')
  end
end
