# Parser.Workaround

![Test Status](https://github.com/pithub/elm-parser-bug-workaround/actions/workflows/tests.yml/badge.svg)

Package with workarounds for [a bug](https://github.com/elm/parser/issues/53)
in [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest).

## Modules

- [`Parser.Workaround`](https://package.elm-lang.org/packages/pithub/elm-parser-bug-workaround/1.0.0/Parser-Workaround)  
    to be used together with elm/parser's
    [`Parser`](https://package.elm-lang.org/packages/elm/parser/latest/Parser) module

- [`Parser.Advanced.Workaround`](https://package.elm-lang.org/packages/pithub/elm-parser-bug-workaround/1.0.0/Parser-Advanced-Workaround)  
    to be used together with elm/parser's
    [`Parser.Advanced`](https://package.elm-lang.org/packages/elm/parser/latest/Parser-Advanced) module


## Tests

This package uses [elm-test](https://github.com/elm-explorations/test),
[elm-verify-examples](https://github.com/stoeffel/elm-verify-examples), and
[elm-review](https://github.com/jfmengels/elm-review).
You can run the tests with:

`npm ci`  
`npm test`

## Contributing

Feedback and pull requests are very welcome.
You can reach the maintainer at @pit on slack, or @pithub on github.

## License

The source code for this package is released under the terms of the MIT license.
See the `LICENSE` file.
