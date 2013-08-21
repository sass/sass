Reproducing Threadsafety Issues
===============================

Write a script that exercises the code with the threadsafety issue and
place it in the `issues` directory. Write code that is as targeted as
possible. This increases the chances of reproducing the condition in
reasonable timeframes.

Some conditions may only occur at startup with a fresh ruby runtime so
the `test_threadsafety.sh` shell script allows you to run the script
many times with each retry being done with a fresh ruby runtime. The
number of retries can be set with the `-r` option (default 100). If you
do not need a fresh ruby runtime to reproduce this, you can call the
`multi_threaded_runner.rb` script as an executable instead.

In order to handle deadlocks and hangs, the code running in each thread
will only be terminated after 10 seconds. If you need longer, use the
`-w` command line switch. When this occurs, the test fails with an
error.

If the issue is some kind of data corruption, the script should detect
this and raise an exception when it occurs.

If you have some code that needs to run before the threads are started,
use the `-s` option to setup and pass it a path to a setup script.

The number of concurrent threads can be set with the `-t` option.
Usually, you'll want to set this to the number of CPUs on your machine.

Thanks to the GIL, It's less likely that a threadsafety issue will
exhibit on MRI than on Rubinius or JRuby. If the issue is ruby
implementation specific, please note this in the issue script.

Running a Script
----------------

    bundle exec ./test_threadsafety.sh -w <# of seconds for threads to run> -t <# of threads> -r <# of retries> <script to run in each thread>

For example:

    # Start ruby 1,000 times and load sass 4 times in each startup.
    bundle exec ./test_threadsafety.sh -w 60 -t 4 -r 1000 issues/sass_boot.rb
    
    # In a single ruby vm, run issues/dependencies_cache.rb in 4 threads after executing the sass_boot.rb script.
    bundle exec ./test_threadsafety.rb -w 60 -t 4 -s issues/sass_boot.rb issues/dependencies_cache.rb
