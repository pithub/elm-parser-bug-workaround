module Parser.Advanced.Workaround exposing
    ( lineCommentBefore, lineCommentAfter, multiCommentBefore, multiCommentAfter
    , chompUntilBefore, chompUntilAfter, chompUntilEndOrBefore, chompUntilEndOrAfter
    )

{-| Workarounds for [a bug](https://github.com/elm/parser/issues/53) in
[`Parser.Advanced`](https://package.elm-lang.org/packages/elm/parser/latest/Parser-Advanced).

---

**Everything here works just like in the
[`Parser.Workaround`](Parser-Workaround) module, except that `String`
arguments become `Token` arguments, and you need to provide a `Problem` for
certain scenarios.**

---


# Bug Workarounds

---

See the [`Parser.Workaround`](Parser-Workaround) documentation
for a description of the [problem](Parser-Workaround#the-problem)
and [guidelines](Parser-Workaround#guidelines) for the use of the workaround parsers.

---

@docs lineCommentBefore, lineCommentAfter, multiCommentBefore, multiCommentAfter
@docs chompUntilBefore, chompUntilAfter, chompUntilEndOrBefore, chompUntilEndOrAfter

-}

import Parser.Advanced as Parser exposing (Parser)
import Parser.Advanced.Extra as Extra



-- CHOMP UNTIL


{-| Just like [`Parser.Advanced.chompUntil`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#chompUntil)
except it consistently stops before the token.
-}
chompUntilBefore : Parser.Token x -> Parser c x ()
chompUntilBefore ((Parser.Token _ expecting) as token) =
    Parser.chompUntil token
        |> beforeTokenParser expecting


{-| Just like [`Parser.Advanced.chompUntil`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#chompUntil)
except it consistently stops after the token.
-}
chompUntilAfter : Parser.Token x -> Parser c x ()
chompUntilAfter token =
    chompUntilBefore token
        |> afterTokenParser token



-- CHOMP UNTIL END OR


{-| Just like [`Parser.Advanced.chompUntilEndOr`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#chompUntilEndOr)
except it consistently stops before the token (if it is found).
-}
chompUntilEndOrBefore : Parser.Token x -> Parser c x ()
chompUntilEndOrBefore (Parser.Token str expecting) =
    Parser.chompUntilEndOr str
        |> beforeTokenParser expecting


{-| Just like [`Parser.Advanced.chompUntilEndOr`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#chompUntilEndOr)
except it consistently stops after the token (if it is found).
-}
chompUntilEndOrAfter : Parser.Token x -> Parser c x ()
chompUntilEndOrAfter token =
    chompUntilEndOrBefore token
        |> afterTokenParser token



-- MULTI COMMENT


{-| Just like [`Parser.Advanced.multiComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#multiComment)
except it consistently stops before the last closing token.
-}
multiCommentBefore : Parser.Token x -> Parser.Token x -> Parser.Nestable -> Parser c x ()
multiCommentBefore open ((Parser.Token str expecting) as close) nestable =
    let
        parser : Parser c x ()
        parser =
            Parser.multiComment open close nestable
    in
    case nestable of
        Parser.Nestable ->
            Extra.bind (Extra.getParserOutcome parser) <|
                \( source, outcome ) ->
                    case outcome of
                        Ok ( _, offset ) ->
                            chompN expecting source (offset - String.length str)

                        Err _ ->
                            errorAtEnd expecting

        Parser.NotNestable ->
            parser
                |> beforeTokenParser expecting


{-| Just like [`Parser.Advanced.multiComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#multiComment)
except it consistently stops after the last closing token.
-}
multiCommentAfter : Parser.Token x -> Parser.Token x -> Parser.Nestable -> Parser c x ()
multiCommentAfter open close nestable =
    multiCommentBefore open close nestable
        |> afterTokenParser close



-- LINE COMMENT


{-| Just like [`Parser.Advanced.lineComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#lineComment)
except it consistently stops before the linefeed character.
-}
lineCommentBefore : Parser.Token x -> Parser c x ()
lineCommentBefore ((Parser.Token _ expecting) as token) =
    Parser.lineComment token
        |> beforeTokenParser expecting


{-| Just like [`Parser.Advanced.lineComment`](https://package.elm-lang.org/packages/elm/parser/latest/Parser.Advanced#lineComment)
except it consistently stops after the linefeed character.
-}
lineCommentAfter : Parser.Token x -> Parser c x ()
lineCommentAfter ((Parser.Token _ expecting) as token) =
    lineCommentBefore token
        |> afterTokenParser (Parser.Token "\n" expecting)



-- UTILS


beforeTokenParser : x -> Parser c x () -> Parser c x ()
beforeTokenParser expecting parser =
    Extra.bind (Extra.getParserOutcome parser) <|
        \( source, outcome ) ->
            case outcome of
                Ok ( _, offset ) ->
                    chompN expecting source offset

                Err _ ->
                    parser


afterTokenParser : Parser.Token x -> Parser c x () -> Parser c x ()
afterTokenParser ((Parser.Token _ expecting) as token) beforeParser =
    beforeParser
        |> Extra.followedBy
            (Parser.oneOf
                [ Parser.end expecting
                , Parser.token token
                ]
            )


chompN : x -> String -> Int -> Parser c x ()
chompN expecting source count =
    Parser.token (Parser.Token (String.left count source) expecting)


errorAtEnd : x -> Parser c x a
errorAtEnd expecting =
    Parser.chompWhile (\_ -> True)
        |> Extra.followedBy (Parser.problem expecting)
