require "sass/script/functions"

module Sass::Script::Functions

  # returns one of two values based on the truth value of the first argument.
  #
  # @example
  #   if(true, 1px, 2px) => 1px
  #   if(false, 1px, 2px) => 2px
  # @param truth [Bool] the expression to be evaluated for truth
  # @param if_true will be returned if the truth value is true.
  # @param if_false will be returned if the truth value is false.
  def if(truth, if_true, if_false)
    if truth.to_bool
      if_true
    else
      if_false
    end
  end

end

