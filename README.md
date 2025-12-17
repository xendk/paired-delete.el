# Paired Delete

This is a companion mode to
[`smartparens`](https://github.com/Fuco1/smartparens) that ensures that
`smartparens` pairs are deleted in, well, pairs.

While `smartparens` has some auto-deletion of pairs, it only works on
empty or just wrapped pairs. `paired-delete-mode` deletes the matching
pair, regardless of content.

For instance, deleting any of the parens in:

```
func("test");
```

Using either backspace or delete, will result in:

```
func"test";
```

As `paired-delete-mode` relies on `smartparens`, it'll handle any
nesting of pairs properly, but it only handles single character pairs.

In order to not get in a fight with `smartparens`, the variables
`sp-autodelete-closing-pair`, `sp-autodelete-opening-pair` and
`sp-autodelete-pair` should all be set to `nil`.

# Usage

Load the package and turn on either `global-paired-delete-mode`, or
`paired-delete-mode` in individual buffers.

The customize-variable `paired-delete-ignore-modes-list` configures
modes where `paired-delete-mode` shouldn't be enabled by the global
mode.
