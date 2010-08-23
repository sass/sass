# Frequently Asked Questions

* Table of contents
{:toc}

## Can I use a variable from my controller in my Sass file?
{#q-ruby-code}

No. Sass files aren't views.
They're compiled once into static CSS files,
then left along until they're changed and need to be compiled again.
Not only don't you want to be running a full request cycle
every time someone requests a stylesheet,
but it's not a great idea to put much logic in there anyway
due to how browsers handle them.

If you really need some sort of dynamic CSS,
you can define your own {Sass::Script::Functions Sass functions} using Ruby
that can access the database or other configuration.
*Be aware when doing this that Sass files are by default only compiled once
and then served statically.*

If you really, really need to compile Sass on each request,
first make sure you have adequate caching set up.
Then you can use {Sass::Engine} to render the code,
using the {file:SASS_REFERENCE.md#custom-option `:custom` option}
to pass in data that {Sass::Script::Functions::EvaluationContext#options can be accessed}
from your Sass functions.

# You still haven't answered my question!

Sorry! Try looking at the [Sass](http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html) reference,
If you can't find an answer there,
feel free to ask in `#sass` on irc.freenode.net
or send an email to the [mailing list](http://groups.google.com/group/sass-lang).
