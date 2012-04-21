# A wrapper around the Listen gem. Adds support for listening to individual
# files, as well as a somewhat cleaner event dispatch API.
#
# @private
class Sass::Plugin::Listener
  def initialize
    @directories = {}
    yield self
    begin
      start!
    rescue Exception => e
      raise e unless e.is_a?(Interrupt)
    end
  end

  def directory(path, events)
    (@directories[path] ||= []) << events
  end

  def file(path, events)
    file_base = File.basename(path)
    directory(File.dirname(path), {
        :modified => file_event_fn(events[:modified], file_base),
        :added => file_event_fn(events[:added], file_base),
        :removed => file_event_fn(events[:removed], file_base)
      })
  end

  def start!
    listener = Listen::MultiListener.new(*@directories.keys) do |modified, added, removed|
      modified = modified.group_by {|path| File.dirname(path)}
      added = added.group_by {|path| File.dirname(path)}
      removed = removed.group_by {|path| File.dirname(path)}

      @directories.each do |dir, events|
        events.each do |e|
          run_events(modified[dir], e[:modified], dir)
          run_events(added[dir], e[:added], dir)
          run_events(removed[dir], e[:removed], dir)
        end
      end
    end.start
  end

  private

  def file_event_fn(event, file_base)
    lambda do |dir, base|
      next unless event
      next unless base == file_base
      event.call
    end
  end

  def run_events(paths, event, path)
    return if paths.nil? || event.nil?
    paths.each {|p| event[File.dirname(p), File.basename(p)]}
  end
end
