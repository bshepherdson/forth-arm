# Forth ARM - Forth 2012 Standard Compliance

This document describes the implementation of the Forth 2012 Standard by Forth ARM.

## Portions of the Standard implemented

Forth ARM implements the `CORE`, `CORE EXT`, `BLOCK`, `BLOCK EXT`, `FILE`, and `FILE EXT` portions of the Standard, except as noted below.

## Missing words from the Standard sections above

### Missing from `CORE`

- Number conversion words: `<#`, `#>`, `#`, `#S`, `HOLD`, `SIGN`
- Some double-cell math words: `*/`, `*/MOD`, `M*`, `2*`, `2!`, `2@`, `2/`, `UM*`, `UM/MOD`
- Parsing-related words: `>NUMBER`
- `FM/MOD` and `SM/REM`
- `S>D` and `D>S`
- `SOURCE`
- `TYPE` is my `TELL`, rename that.
- `[CHAR]`

### Missing from `CORE EXT`

- `.(`
- `2>R`, `2R>`, `2R@`
- `:NONAME`
- `?DO`
- `ACTION-OF`, `DEFER`, `DEFER@`, `DEFER!`, `IS`.
- `BUFFER:` (replaces `ARRAY`?)
- `C"`
- `COMPILE,`
- `ERASE`
- `HOLDS` (numeric output)
- `MARKER`
- `PAD`
- `PARSE`
- `PARSE-NAME`
- `SAVE-INPUT` and `RESTORE-INPUT`
- `S\"`
- `SOURCE-ID`
- `UNUSED`
- `VALUE` and `TO`.

Also need to check if `[COMPILE]` in my code and the standard match, and how they relate to `POSTPONE`.
