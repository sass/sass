File.open('Makefile', 'w') do |f|
  f.puts("install:\n\trake freeze_root")
end
