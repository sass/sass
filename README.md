The `haml-pages` and `sass-pages` branches
contain the code for the Haml and Sass websites,
[haml-lang.com](http://haml-lang.com) and [sass-lang.com](http://sass-lang.com).
The websites are automatically updated when the branches are pushed
to [github.com/nex3/haml](http://github.com/nex3/haml).

These pages are compiled with [StaticMatic](http://staticmatic.rubyforge.org),
and use the [Compass](http://compass-style.org/) Sass framework.
To compile, run

    staticmatic build .

To set up a local server for viewing updates, run

    staticmatic preview .
