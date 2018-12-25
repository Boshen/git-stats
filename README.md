# git-stats

Development:
```
stack build --fast --exec "stack run git-stats . +RTS -N"
```

## Notes

* getting blame files is sped up by `mapConcurrently` from the `async` library
* parsing blame files is sped up by `\`using\` parList rseq` from Control.Parallel.Strategies
* everything is done in `Text`, noticeably `readCreateProcessWithExitCode` from `process-extras`
* blames files are parsed with `megaparsec`
* `prettyprinter` is used for cli printing, with colors
