require "rubocop/cop/style/surrounding_space"

module Rubocop
  module Cop
    module Style
      # Common functionality for checking surrounding space.
      # # This is monkeypatching an existing class.
      class SpaceAroundBlockBraces < Cop
        MSG_RIGHT_SPACE = "Space found to the left of '}'."
        MSG_LEFT_SPACE = "Space found to the right of '{'."
        def investigate(processed_source)
          return unless processed_source.ast
          @processed_source = processed_source

          processed_source.tokens.each_cons(2) do |t1, t2|
            next if ([t1.pos, t2.pos] - positions_not_to_check).size < 2

            type1, type2 = t1.type, t2.type
            check(t1, t2, MSG_LEFT) if type2 == :tLCURLY
            if type1 == :tLCURLY
              if type2 == :tPIPE && cop_config['NoSpaceBeforeBlockParameters']
                check_no_space(t1, t2, MSG_PIPE)
              elsif cop_config['NoSpaceAfterOpeningCurly']
                check_no_space(t1, t2, MSG_LEFT_SPACE)
              else
                check(t1, t2, MSG_LEFT)
              end
            end
            if type2 == :tRCURLY
              if cop_config['NoSpaceBeforeClosingCurly']
                check_no_space(t1, t2, MSG_RIGHT_SPACE)
              else
                check(t1, t2, MSG_RIGHT) 
              end
            end
          end
        end

        def check_no_space(t1, t2, msg)
          convention(nil, t1.pos, msg) if space_between?(t1, t2)
        end
      end
    end
  end
end

Rubocop::Config.default_configuration["SpaceAroundBlockBraces"]["NoSpaceBeforeClosingCurly"] = false
Rubocop::Config.default_configuration["SpaceAroundBlockBraces"]["NoSpaceAfterOpeningCurly"] = false
