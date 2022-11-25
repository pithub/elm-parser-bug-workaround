module Tests.Parser.Advanced.Workaround exposing (suite)

import Expect
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Parser.Advanced.Workaround as Workaround
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parser.Advanced.Extra"
        [ Test.describe "position bug" positionBugTests
        , Test.describe "chompUntilBefore" chompUntilBeforeTests
        , Test.describe "chompUntilAfter" chompUntilAfterTests
        , Test.describe "chompUntilEndOrBefore" chompUntilEndOrBeforeTests
        , Test.describe "chompUntilEndOrAfter" chompUntilEndOrAfterTests
        , Test.describe "multiCommentBefore" multiCommentBeforeTests
        , Test.describe "multiCommentAfter" multiCommentAfterTests
        , Test.describe "lineCommentBefore" lineCommentBeforeTests
        , Test.describe "lineCommentAfter" lineCommentAfterTests
        ]


positionBugTests : List Test
positionBugTests =
    [ Test.test "chompWhile -> no error" <|
        \() ->
            Parser.run
                (Parser.chompWhile ((/=) '|') |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 4 ))
    , Test.test "chompUntil error -> normal error" <|
        \() ->
            Parser.run
                (Parser.chompUntil (Parser.Token "not there" (NormalError "token not there")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Err
                        [ { row = 3
                          , col = 8
                          , problem = NormalError "token not there"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "chompUntil ok -> position error" <|
        \() ->
            Parser.run
                (Parser.chompUntil (Parser.Token "|" (NormalError "token |")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Err
                        [ { row = 3
                          , col = 5
                          , problem =
                                PositionError
                                    { positionAtOffset = ( 3, 4 )
                                    , actualPosition = ( 3, 5 )
                                    }
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "chompUntilEndOr not found -> no error" <|
        \() ->
            Parser.run
                (Parser.chompUntilEndOr "not there" |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 8 ))
    , Test.test "chompUntilEndOr found -> position error" <|
        \() ->
            Parser.run
                (Parser.chompUntilEndOr "|" |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Err
                        [ { row = 3
                          , col = 5
                          , problem =
                                PositionError
                                    { positionAtOffset = ( 3, 4 )
                                    , actualPosition = ( 3, 5 )
                                    }
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "multiComment nestable error -> normal error" <|
        \() ->
            Parser.run
                (Parser.multiComment
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.Nestable
                    |> checked
                )
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = NormalError "close"
                          , contextStack = []
                          }
                        , { row = 1
                          , col = 14
                          , problem = NormalError "open"
                          , contextStack = []
                          }
                        , { row = 1
                          , col = 14
                          , problem = NormalError "close"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "multiComment nestable ok -> no error" <|
        \() ->
            Parser.run
                (Parser.multiComment
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.Nestable
                    |> checked
                )
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 14 ))
    , Test.test "multiComment not nestable error -> normal error" <|
        \() ->
            Parser.run
                (Parser.multiComment
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.NotNestable
                    |> checked
                )
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = NormalError "close"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "multiComment not nestable ok -> position error" <|
        \() ->
            Parser.run
                (Parser.multiComment
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.NotNestable
                    |> checked
                )
                "{- comment -} rest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem =
                                PositionError
                                    { positionAtOffset = ( 1, 12 )
                                    , actualPosition = ( 1, 14 )
                                    }
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "lineComment error -> normal error" <|
        \() ->
            Parser.run
                (Parser.lineComment (Parser.Token "--" (NormalError "start")) |> checked)
                "{- comment\nrest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = NormalError "start"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "lineComment ok at end -> no error" <|
        \() ->
            Parser.run
                (Parser.lineComment (Parser.Token "--" (NormalError "start")) |> checked)
                "-- comment"
                |> Expect.equal
                    (Ok ( 1, 11 ))
    , Test.test "lineComment ok before end -> position error" <|
        \() ->
            Parser.run
                (Parser.lineComment (Parser.Token "--" (NormalError "start")) |> checked)
                "-- comment\nrest"
                |> Expect.equal
                    (Err
                        [ { row = 2
                          , col = 1
                          , problem =
                                PositionError
                                    { positionAtOffset = ( 1, 11 )
                                    , actualPosition = ( 2, 1 )
                                    }
                          , contextStack = []
                          }
                        ]
                    )
    ]


chompUntilBeforeTests : List Test
chompUntilBeforeTests =
    [ Test.test "chompUntilBefore error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilBefore (Parser.Token "not there" (NormalError "token not there")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Err
                        [ { row = 3
                          , col = 8
                          , problem = NormalError "token not there"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "chompUntilBefore ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilBefore (Parser.Token "|" (NormalError "token |")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 4 ))
    ]


chompUntilAfterTests : List Test
chompUntilAfterTests =
    [ Test.test "chompUntilAfter error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilAfter (Parser.Token "not there" (NormalError "token not there")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Err
                        [ { row = 3
                          , col = 8
                          , problem = NormalError "token not there"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "chompUntilAfter ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilAfter (Parser.Token "|" (NormalError "token |")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 5 ))
    ]


chompUntilEndOrBeforeTests : List Test
chompUntilEndOrBeforeTests =
    [ Test.test "chompUntilEndOrBefore not found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrBefore (Parser.Token "not there" (NormalError "token not there")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 8 ))
    , Test.test "chompUntilEndOrBefore found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrBefore (Parser.Token "|" (NormalError "token |")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 4 ))
    ]


chompUntilEndOrAfterTests : List Test
chompUntilEndOrAfterTests =
    [ Test.test "chompUntilEndOrAfter not found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrAfter (Parser.Token "not there" (NormalError "token not there")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 8 ))
    , Test.test "chompUntilEndOrAfter found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrAfter (Parser.Token "|" (NormalError "token |")) |> checked)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 5 ))
    ]


multiCommentBeforeTests : List Test
multiCommentBeforeTests =
    [ Test.test "multiCommentBefore nestable error -> simplified normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.Nestable
                    |> checked
                )
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = NormalError "close"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "multiCommentBefore nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.Nestable
                    |> checked
                )
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 12 ))
    , Test.test "multiCommentBefore not nestable error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.NotNestable
                    |> checked
                )
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = NormalError "close"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "multiCommentBefore not nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.NotNestable
                    |> checked
                )
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 12 ))
    ]


multiCommentAfterTests : List Test
multiCommentAfterTests =
    [ Test.test "multiCommentAfter nestable error -> simplified normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.Nestable
                    |> checked
                )
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = NormalError "close"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "multiCommentAfter nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.Nestable
                    |> checked
                )
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 14 ))
    , Test.test "multiCommentAfter not nestable error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.NotNestable
                    |> checked
                )
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = NormalError "close"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "multiCommentAfter not nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter
                    (Parser.Token "{-" (NormalError "open"))
                    (Parser.Token "-}" (NormalError "close"))
                    Parser.NotNestable
                    |> checked
                )
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 14 ))
    ]


lineCommentBeforeTests : List Test
lineCommentBeforeTests =
    [ Test.test "lineCommentBefore error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentBefore (Parser.Token "--" (NormalError "start")) |> checked)
                "{- comment\nrest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = NormalError "start"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "lineCommentBefore ok at end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentBefore (Parser.Token "--" (NormalError "start")) |> checked)
                "-- comment"
                |> Expect.equal
                    (Ok ( 1, 11 ))
    , Test.test "lineCommentBefore ok before end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentBefore (Parser.Token "--" (NormalError "start")) |> checked)
                "-- comment\nrest"
                |> Expect.equal
                    (Ok ( 1, 11 ))
    ]


lineCommentAfterTests : List Test
lineCommentAfterTests =
    [ Test.test "lineCommentAfter error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentAfter (Parser.Token "--" (NormalError "start")) |> checked)
                "{- comment\nrest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = NormalError "start"
                          , contextStack = []
                          }
                        ]
                    )
    , Test.test "lineCommentAfter ok at end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentAfter (Parser.Token "--" (NormalError "start")) |> checked)
                "-- comment"
                |> Expect.equal
                    (Ok ( 1, 11 ))
    , Test.test "lineCommentAfter ok before end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentAfter (Parser.Token "--" (NormalError "start")) |> checked)
                "-- comment\nrest"
                |> Expect.equal
                    (Ok ( 2, 1 ))
    ]



-- HELPERS


type TestError
    = NormalError String
    | PositionError
        { positionAtOffset : ( Int, Int )
        , actualPosition : ( Int, Int )
        }


checked : Parser c TestError a -> Parser c TestError ( Int, Int )
checked parser =
    Parser.succeed identity
        |. parser
        |. checkPosition
        |= Parser.getPosition


checkPosition : Parser c TestError ()
checkPosition =
    withSourceOffsetPosition <|
        \source offset actualPosition ->
            let
                positionAtOffset : ( Int, Int )
                positionAtOffset =
                    getPositionAtOffset source offset
            in
            if actualPosition == positionAtOffset then
                Parser.succeed ()

            else
                Parser.problem <|
                    PositionError
                        { positionAtOffset = positionAtOffset
                        , actualPosition = actualPosition
                        }


withSourceOffsetPosition : (String -> Int -> ( Int, Int ) -> Parser c x a) -> Parser c x a
withSourceOffsetPosition callback =
    Parser.getSource
        |> Parser.andThen
            (\source ->
                Parser.getOffset
                    |> Parser.andThen
                        (\offset ->
                            Parser.getPosition
                                |> Parser.andThen
                                    (\position ->
                                        callback source offset position
                                    )
                        )
            )


getPositionAtOffset : String -> Int -> ( Int, Int )
getPositionAtOffset source offset =
    Parser.run
        (Parser.chompWhile (\_ -> True) |> Parser.andThen (\() -> Parser.getPosition))
        (String.left offset source)
        |> Result.withDefault ( 0, 0 )
