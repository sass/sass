The `sass-pages` branch contains the code for the Sass website,
[sass-lang.com](http://sass-lang.com). The website is automatically updated when
the branch is pushed to [github.com/nex3/sass](http://github.com/nex3/sass).

These pages are compiled with [StaticMatic](http://staticmatic.rubyforge.org),
and use the [Compass](http://compass-style.org/) Sass framework.
The Haml and Sass documentation uses [YARD](http://yard.soen.ca/).
To compile everything, run

    rake build

To set up a local server for viewing updates for the site, run

    rake preview
