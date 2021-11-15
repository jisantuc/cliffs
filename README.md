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

| Script Name | Description                                         |
| :---------- | --------------------------------------------------- |
| release     | Create tags and CHANGELOG entries for a new release |
| bootstrap   | Set stuff up                                        |
| server      | Start servers                                       |
| update      | Update dependencies and containers                  |


## Releases

Relases are handled by [`chan`] and GitHub Actions. To create a new release:

- rotate the changelog: `chan release v<VERSION>`
- bump versions in `package.yaml` and `cliffs.cabal` (the second with `hpack`)
- commit the release: `git commit -am "Update changelog for <VERSION>`
- tag the release: `git tag -S -a v<VERSION> -m "Release v<VERSION>"`
- push the tag: `git push origin --tags v<VERSION>`
- create the GitHub release after the build completes

Everything up to creating the GitHub release after the build is handled by the
`scripts/release` script. The GitHub action will create the release itself.

After the release exists, you should:

- paste the changelog for this release
- add an installation command like: `nix-env -i -f
  https://github.com/jisantuc/cliffs/archive/refs/tags/VERSION.zip`. This
  command will work across platforms where `nix` is installed, so MacOS / Linux
  / NixOS / anyone else with nix should be able to call the install command and
  get a working executable.

[`brick`]: https://github.com/jtdaugherty/brick
[scripts to rule them all]:
https://github.com/github/scripts-to-rule-them-all
[`chan`]: https://www.npmjs.com/package/@geut/chan