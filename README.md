# git-stats

Development:
```
stack build --fast --exec "stack run git-stats . +RTS -N"
```

## Example output

```
   Lines     Adds     Dels  Commits    Files Author
   17639    34121    25854      160      205 AAA
    7087    10744     2209       15       90 BBB
    6957    17738     3995       77      134 CCC
    4842     8946      458       67       41 DDD
    1389     2987      765        7       46 EEE
    1240     2965     1175       12       22 FFF
    1043     3768     2196       24       38 GGG
     511     1819      404        3       23 HHH
     262      263       72        2        9 III
     187     1973     1634        3       14 JJJ
      64       64       28        1        1 KKK
      35       60        1        1        5 LLL
      25       62        2        2        2 MMM
```

## Notes

* getting blame files is sped up by `mapConcurrently` from the `async` library
* parsing blame files is sped up by `\`using\` parList rseq` from Control.Parallel.Strategies
* everything is done in `Text`, noticeably `readCreateProcessWithExitCode` from `process-extras`
* blames files are parsed with `megaparsec`
* `prettyprinter` is used for cli printing, with colors
