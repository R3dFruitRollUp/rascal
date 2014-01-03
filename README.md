```
                              __ 
.----.---.-.-----.----.---.-.|  |
|   _|  _  |__ --|  __|  _  ||  |
|__| |___._|_____|____|___._||__|
                                 
```

[![Build Status](https://travis-ci.org/soli/rascal.png)](https://travis-ci.org/soli/rascal)

rascal is a command-line client for [reddit](http://www.reddit.com/).

It should work on any platform where Haskell runs (Linux, Mac OSX and Windows
through MinGW), and aims at making available the whole reddit API (we're not
there yet…).

It sports colors, that should soon become configurable, and might not work
under Windows (based on ANSI escape sequences, through
[ansi-terminal](https://github.com/batterseapower/ansi-terminal)).

By default it will open [/r/haskell](http://www.reddit.com/r/haskell/new)
sorted by "new", but you can provide another subreddit as argument on the
command line

```
$ rascal python
```

It is also possible to define the default subreddit and the default sort
option in an INI-style configuration file named `.rascalrc` in your HOME
directory. For instance, it might contain the following (case-sensitive!):

```
subreddit: programming
linkSort = controversial
commentSort: new
pageComments = True
```

Here is what rascal looks like when started, and when a _self_ link (marked
with a '♦') is chosen (`N` was pressed in this case). Non-self links and links
detected in posts will be opened in your default browser.

![screenshot](https://github.com/soli/rascal/raw/master/screenshot.png)

Comments are shown threaded, with the order defined in the configuration file
(defaulting to `new`), and one page at a time if `pageComments` is true
(default). Comments, like posts, are formatted to avoid cutting words when
possible.

![threads](https://github.com/soli/rascal/raw/master/threads.png)

### Notes

In case you wonder, my iTerm2 is using the dark
[solarized](https://github.com/altercation/solarized) color scheme. The ASCII
title was generated with `figlet -f chunky`. And the name comes from my own
pronunciation of `/r/haskell`.

### Planned Features

* Configuration for colors
* OAuth2 login to reddit (subscribed/saved/etc.)
* more tests

## Disclaimer

Please note that rascal is my first Haskell project. As such, it is as much a
fun experiment for me to discover the ecosystem and the idioms, than a really
useful project. TL;DR: YMMV.
