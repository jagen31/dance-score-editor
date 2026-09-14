# score-dance-editor

Two embedded [DrRacket](https://docs.racket-lang.org/drracket/) editors — a
little **musical score** and a single **dance pose** (a tonart *arm-diagram*) —
that you insert right into the definitions window, edit graphically, and turn
into the equivalent tonart art code.

## Install

From this directory:

```sh
raco pkg install
```

Restart DrRacket. A new **Insert Art** menu appears.

## Use

- **Insert Art ▸ Score editor** drops a staff into your file. Click a cell to
  toggle a note; columns are beats, rows are diatonic steps (the five lines are
  the treble staff, E4–F5).
- **Insert Art ▸ Dance pose** drops a stick figure. Click the left/right half to
  point that arm at the clock position of your click (12 = up, 3 = right,
  6 = down, 9 = left); click the label strip at the bottom to cycle the facing.
- **Insert Art ▸ Copy selected editor's art code** puts the selected editor's
  code on the clipboard, e.g.

  ```
  at [interval 0 1]: note e 0 4
  at [interval 1 2]: note g 0 4
  ```

  or

  ```
  arm-diagram 9 3
  facing left
  ```

Both editors **persist in the saved `.rkt` file** and reopen as editors.

## Status

v0.1 — the editors are authoring aids: draw, then copy the art code into a
program. They do not yet expand *as* code in place. Next steps: richer note
durations, multiple voices, and a reader so a snip evaluates to its forms
directly.
