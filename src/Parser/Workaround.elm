module Parser.Workaround exposing
    ( lineCommentBefore, lineCommentAfter, multiCommentBefore, multiCommentAfter
    , chompUntilBefore, chompUntilAfter, chompUntilEndOrBefore, chompUntilEndOrAfter
    )

{-| Workarounds for [a bug](https://github.com/elm/parser/issues/53) in
[`Parser`](https://package.elm-lang.org/packages/elm/parser/latest/Parser).


## The Problem

The Elm Parser internally keeps track of the current position in two ways:

  - as a row and a column (like a code editor)
  - as an offset into the source string.

See the [Positions](https://package.elm-lang.org/packages/elm/parser/latest/Parser#positions)
chapter in the Parser documentation for more details.

Normally both kinds of position infos (row and column vs. offset) are in sync with each other.
(For a given source string, you can calculate both row and column from the offset and vice versa.)

There's [a bug](https://github.com/elm/parser/issues/53) in the Parser code though.
The following parsers break this synchronicity:
[`lineComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#lineComment),
[`multiComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#multiComment),
[`chompUntil`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#chompUntil), and
[`chompUntilEndOr`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#chompUntilEndOr).
They set...

  - row and column **after** the (closing) token
  - the offset **before** the (closing) token

Here's an example with [`chompUntil`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#chompUntil):

    import Parser exposing ((|.), (|=), Parser)

    testParser : Parser { row : Int, col : Int, offset : Int }
    testParser =
        Parser.succeed (\row col offset -> { row = row, col = col, offset = offset })
            |. Parser.chompUntil "token"
            |= Parser.getRow
            |= Parser.getCol
            |= Parser.getOffset

    Parser.run testParser "< token >"
    --> Ok { row = 1, col = 8, offset = 2 }

The state after the test parser is run:

  - row = 1, col = 8 (corresponding to offset = 7) --> **after** the token
  - offset = 2 (corresponding to row = 1, col = 3) --> **before** the token


## Workaround

As a workaround, this package offers `xxxBefore` and `xxxAfter` parsers which consistently position
both row/column and offset either before or after the (closing) token.

  - [`lineCommentBefore`](#lineCommentBefore) and [`lineCommentAfter`](#lineCommentAfter)
  - [`multiCommentBefore`](#multiCommentBefore) and [`multiCommentAfter`](#multiCommentAfter)
  - [`chompUntilBefore`](#chompUntilBefore) and [`chompUntilAfter`](#chompUntilAfter)
  - [`chompUntilEndOrBefore`](#chompUntilEndOrBefore) and [`chompUntilEndOrAfter`](#chompUntilEndOrAfter)

Why are there two different workarounds for each buggy parser?

On the one hand, if you already have working parsers and don't use the row or column information,
then you can replace them with the `xxxBefore` variants and they will continue to work.
(There's one exception for the `multiComment` parser though, [see below](#exception).)

On the other hand, most often the `xxxAfter` variants are easier to use,
because you don't need to chomp the (closing) token yourself.
So if you write new parsers, you'll likely want to use the `xxxAfter` variants.
Plus, if the bug will be fixed, it can be assumed that the
fixed parsers will work like the `xxxAfter` variants.
(If you want to know why, you can look at the description of
[this pull request](https://github.com/elm/parser/pull/54).)


## Guidelines

If you are unsure whether to use the `xxxBefore` or the `xxxAfter` parsers
or whether to use the workarounds at all, you could follow these guidelines:

  - If you already use one or more of the four buggy parsers:
      - If they work the way you want:
          - If it's likely that you'll need to modify your parsers:
              - Switch to the `xxxBefore` workarounds.
                Then you don't have to change your existing parsers,
                but the workarounds maintain an internally consistent state.
                This makes it easier to combine them with other parsers later.
                (There's one exception for the `multiComment` parser though, [see below](#exception).)
          - If it's unlikely that you'll need to modify your parsers:
              - Don't change anything (never touch a running system).
      - If they don't work the way you want:
          - Switch to the `xxxAfter` workarounds, because often they are easier to use
            and because they implement the intended behavior.
            You'll have to modify the surrounding parsers, but they don't work anyhow.
  - If you don't use one or more of the four buggy parsers yet but are planning to do so:
      - Use the `xxxAfter` workarounds, because often they are easier to use
        and because they implement the intended behavior.
  - If you don't use any buggy parser yet and are not planning to do so:
      - No buggy parser, no need for a workaround 🙂


### Exception

There's one exception to the rules above: if the
[`multiComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#multiComment)
parser is used with
[`Nestable`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#Nestable)
comments, then it isn't affected from the bug.
(For this mode it's implemented differently.)

Therefore this parser should be replaced with the [`multiCommentAfter`](#multiCommentAfter)
workaround to keep the current behavior.


## Parsers

@docs lineCommentBefore, lineCommentAfter, multiCommentBefore, multiCommentAfter
@docs chompUntilBefore, chompUntilAfter, chompUntilEndOrBefore, chompUntilEndOrAfter

-}

import Parser exposing (Parser)
import Parser.Advanced as Advanced
import Parser.Advanced.Workaround as AdvancedWorkaround



-- CHOMP UNTIL


{-| Just like [`Parser.chompUntil`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#chompUntil)
except it consistently stops before the string.
-}
chompUntilBefore : String -> Parser ()
chompUntilBefore str =
    AdvancedWorkaround.chompUntilBefore (toToken str)


{-| Just like [`Parser.chompUntil`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#chompUntil)
except it consistently stops after the string.
-}
chompUntilAfter : String -> Parser ()
chompUntilAfter str =
    AdvancedWorkaround.chompUntilAfter (toToken str)



-- CHOMP UNTIL END OR


{-| Just like [`Parser.chompUntilEndOr`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#chompUntilEndOr)
except it consistently stops before the string (if it is found).
-}
chompUntilEndOrBefore : String -> Parser ()
chompUntilEndOrBefore str =
    AdvancedWorkaround.chompUntilEndOrBefore (toToken str)


{-| Just like [`Parser.chompUntilEndOr`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#chompUntilEndOr)
except it consistently stops after the string (if it is found).
-}
chompUntilEndOrAfter : String -> Parser ()
chompUntilEndOrAfter str =
    AdvancedWorkaround.chompUntilEndOrAfter (toToken str)



-- MULTI COMMENT


{-| Just like [`Parser.multiComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#multiComment)
except it consistently stops before the last closing string.
-}
multiCommentBefore : String -> String -> Parser.Nestable -> Parser ()
multiCommentBefore open close nestable =
    AdvancedWorkaround.multiCommentBefore (toToken open) (toToken close) (toAdvancedNestable nestable)


{-| Just like [`Parser.multiComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#multiComment)
except it consistently stops after the last closing string.
-}
multiCommentAfter : String -> String -> Parser.Nestable -> Parser ()
multiCommentAfter open close nestable =
    AdvancedWorkaround.multiCommentAfter (toToken open) (toToken close) (toAdvancedNestable nestable)



-- LINE COMMENT


{-| Just like [`Parser.lineComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#lineComment)
except it consistently stops before the linefeed character.
-}
lineCommentBefore : String -> Parser ()
lineCommentBefore str =
    AdvancedWorkaround.lineCommentBefore (toToken str)


{-| Just like [`Parser.lineComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#lineComment)
except it consistently stops after the linefeed character.
-}
lineCommentAfter : String -> Parser ()
lineCommentAfter str =
    AdvancedWorkaround.lineCommentAfter (toToken str)



-- UTILS


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)


toAdvancedNestable : Parser.Nestable -> Advanced.Nestable
toAdvancedNestable nestable =
    case nestable of
        Parser.NotNestable ->
            Advanced.NotNestable

        Parser.Nestable ->
            Advanced.Nestable
