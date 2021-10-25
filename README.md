# `cliffs`

`cliffs` is a small CLI built on [`brick`] that makes 
interacting with [scripts to rule them all] (STRTA) easier.

It is oriented around three features that should alleviate some
common pain points:

| Pain Point                                     | `cliffs` help                                                    |
| :--------------------------------------------- | ---------------------------------------------------------------- |
| Remembering what script does what              | Rendering script descriptions next to names                      |
| Remembering what arguments scripts take        | Rendering `--help` output with a hotkey                          |
| Separating help from where you run the command | Providing a small terminal for command submission with the table |

You can see an example by running `cliffs` in this directory. Running the executable will get you an interactive table of scripts like:

```
┏━━━━━━━━━━━━━━━━Scripts━━━━━━━━━━━━━━━━┓
┃   bootstrap   ┃ description goes here ┃
┃   server      ┃ description goes here ┃
┃   update      ┃ description goes here ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

You can press the up / down arrow to select different scripts.