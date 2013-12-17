rascal
======

[![Build Status](https://travis-ci.org/soli/soli.png)](https://travis-ci.org/soli/soli)

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
