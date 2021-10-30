# `cliffs`

`cliffs` is a small CLI built on [`brick`] that makes interacting with [scripts
to rule them all] (STRTA) easier.

It is oriented around three features that should alleviate some common pain
points:

| Pain Point                                     | `cliffs` help                                                    |
| :--------------------------------------------- | ---------------------------------------------------------------- |
| Remembering what script does what              | Rendering script descriptions next to names                      |
| Remembering what arguments scripts take        | Rendering `--help` output with a hotkey                          |
| Separating help from where you run the command | Providing a small terminal for command submission with the table |

You can see an example by running `cliffs` in this directory. Running the
executable will get you an interactive table of scripts like:

```
┏━━━━━━━━━━━━━━━━━━━━━━━Scripts━━━━━━━━━━━━━━━━━━━━━━┓
┃   bootstrap   ┃ Set stuff up                       ┃
┃   server      ┃ Start servers                      ┃
┃   update      ┃ Update dependencies and containers ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

You can press the up / down arrow to select different scripts, which will update
the `Help output` box (not shown in paste above).

## Script descriptions

Scripts can have short descriptions as well as their help text. You can include
such information in a table in a README. Your README must live in a file called
`README.md`. `cliffs` will show the description from the table in the README for
any scripts that have them. To detect the short descriptions automatically, your
README _must_ have a table with two columns named "Script Name" and
"Description" somewhere in the README. In the future this requirement may be
relaxed, for example by taking arguments for the column headers and the README
location, but for now the requirements are strict.

An example table is below:

| Script Name | Description                        |
| :---------- | ---------------------------------- |
| bootstrap   | Set stuff up                       |
| server      | Start servers                      |
| update      | Update dependencies and containers |


[`brick`]: https://github.com/jtdaugherty/brick [scripts to rule them all]:
https://github.com/github/scripts-to-rule-them-all