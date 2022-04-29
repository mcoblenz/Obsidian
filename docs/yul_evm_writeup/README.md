make the pdf with `latexmk --pdf main.tex`; `latexmk -C` cleans up.

the macros file defines `\todo`, which i have used to mark places with more
work to do. if you delete it and recompile the TeX from scratch without
errors, that's good (if not perfect) evidence that there isn't
metacommentary in the text. similarly, citations that haven't been fleshed
out yet cite `TODO` and show up in the output from `latexmk` like this:

```
Latexmk: ====List of undefined refs and citations:
  Citation `TODO' on page 1 undefined on input line 49
  Citation `TODO' on page 2 undefined on input line 93
  Citation `todo' on page 3 undefined on input line 44
  Citation `todo' on page 3 undefined on input line 48
  Citation `todo' on page 3 undefined on input line 50
```

resolving all of those and also not having errors after undefining the
`\todo` command is a good sign that things are alright.

the csvs with benchmark data in them are produced by the following
commands, in section order, when run from the top level of the obsidian
repo. each produces a file named `benchmarks(timestamp).csv`. if you move
them into this directory with the right names they'll get added to the
paper.

python travis_specific/ganache_tests.py -ob AssignLocalAdd PrimOpsEq Return
python travis_specific/ganache_tests.py -ob LinkedListShort LinkedListShortNoGC LinkedListMed LinkedListMedNoGC
python travis_specific/ganache_tests.py -ob SetGetPointer SetGetNestedPointerPassThrough SetGetLogs

the full suite of benchmarks can be generated with `python
travis_specific/ganache_tests.py -b`. that script has a fair few
commandline options, which are described in ` python
travis_specific/ganache_tests.py --help`

this is somewhat redacted since you can break up the benchmarks into
semantic equivalence classes based basically on how much gas they use; it
shows if they really do anything that the optimizer can't riddle out or
touch memory at all. (many of them don't even have fields, so the tracers
are all trivial).
