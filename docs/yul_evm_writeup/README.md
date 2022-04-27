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
