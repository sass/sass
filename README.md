The `haml-pages` and `sass-pages` branches
contain the code for the Haml and Sass websites,
[haml-lang.com](http://haml-lang.com) and [sass-lang.com](http://sass-lang.com).
The websites are automatically updated when the branches are pushed
to [github.com/nex3/haml](http://github.com/nex3/haml).

These pages are compiled with [StaticMatic](http://staticmatic.rubyforge.org),
and use the [Compass](http://compass-style.org/) Sass framework.
The Haml and Sass documentation uses [YARD](http://yard.soen.ca/).
To compile everything, run

    rake build

To set up a local server for viewing updates for the site, run

    rake preview
