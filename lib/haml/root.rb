module Haml
  # The root directory of the Haml source tree.
  # This may be overridden by the package manager
  # if the lib directory is separated from the main source tree.
  # @api public
  ROOT_DIR = File.expand_path("../../..", __FILE__)
end
