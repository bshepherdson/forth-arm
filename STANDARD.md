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

### Missing from `CORE EXT`

- `?DO` - Leaving this for now, it's a minor point.
- `HOLDS` (numeric output)
- `SAVE-INPUT` and `RESTORE-INPUT`
- `S\"`


## Deviations

### `FILE` and `FILE-EXT`

- `(` is not multi-line when working on files.
- Missing `READ-LINE`, `INCLUDED`, `INCLUDE-FILE` and all of `FILE-EXT`
