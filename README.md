# SMS message generator for "ausweis" during quarantine in Moscow

## Table of contents

  * [Goal](#goal)
  * [Installation](#installation)

## Goal

Moscow goverment stated that each person must have an "ausweis" (temporal
permition) to go to the outside. To automatize this process I have decided to
write a simple application which resolves this task.

This little application allows you to generate SMS message which you can send
on **7377** phone number, to get the "ausweis".

The second reason why I have written this application was to demonstrate
a difference between two approaches to write an application:
  * using dependent types
  * and not using them

If you want to see that difference you could change `app/Main.hs` by
comment/uncomment `AD.main` or `AN.main` and reinstall application
with `cabal install`.
The output will be the same because the difference lies inside the source code,
so you probably want to also check a `src` directory too.

## Installation

1. To install this app you __must__ have `cabal` tool and `ghc` compiler
installed which you can get [by `ghcup` here](https://www.haskell.org/ghcup).

How to install `cabal` and `ghc` using `ghcup` more detailed you can read in
[this article](https://gitlab.haskell.org/haskell/ghcup-hs).

2. Just `git clone` repository and `cabal install` it!

```zsh
$ git clone https://github.com/ushfnuk/ausweis.git
$ cd ausweis
$ cabal install
```

Then you could run application with:
```zsh
$ ausweis
```
If of course the path `~/.cabal/bin` is present in the `PATH` environment
variable.
