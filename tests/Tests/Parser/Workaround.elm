module Tests.Parser.Workaround exposing (suite)

import Expect
import Parser exposing (Parser)
import Parser.Workaround as Workaround
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parser.Extra"
        [ Test.describe "chompUntilBefore" chompUntilBeforeTests
        , Test.describe "chompUntilAfter" chompUntilAfterTests
        , Test.describe "chompUntilEndOrBefore" chompUntilEndOrBeforeTests
        , Test.describe "chompUntilEndOrAfter" chompUntilEndOrAfterTests
        , Test.describe "multiCommentBefore" multiCommentBeforeTests
        , Test.describe "multiCommentAfter" multiCommentAfterTests
        , Test.describe "lineCommentBefore" lineCommentBeforeTests
        , Test.describe "lineCommentAfter" lineCommentAfterTests
        ]


chompUntilBeforeTests : List Test
chompUntilBeforeTests =
    [ Test.test "chompUntilBefore error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilBefore "not there" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Err
                        [ { row = 3
                          , col = 8
                          , problem = Parser.Expecting "not there"
                          }
                        ]
                    )
    , Test.test "chompUntilBefore ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilBefore "|" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 4 ))
    ]


chompUntilAfterTests : List Test
chompUntilAfterTests =
    [ Test.test "chompUntilAfter error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilAfter "not there" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Err
                        [ { row = 3
                          , col = 8
                          , problem = Parser.Expecting "not there"
                          }
                        ]
                    )
    , Test.test "chompUntilAfter ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilAfter "|" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 5 ))
    ]


chompUntilEndOrBeforeTests : List Test
chompUntilEndOrBeforeTests =
    [ Test.test "chompUntilEndOrBefore not found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrBefore "not there" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 8 ))
    , Test.test "chompUntilEndOrBefore found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrBefore "|" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 4 ))
    ]


chompUntilEndOrAfterTests : List Test
chompUntilEndOrAfterTests =
    [ Test.test "chompUntilEndOrAfter not found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrAfter "not there" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 8 ))
    , Test.test "chompUntilEndOrAfter found -> no error" <|
        \() ->
            Parser.run
                (Workaround.chompUntilEndOrAfter "|" |> withPosition)
                "a\nbc\nd🙂e|end"
                |> Expect.equal
                    (Ok ( 3, 5 ))
    ]


multiCommentBeforeTests : List Test
multiCommentBeforeTests =
    [ Test.test "multiCommentBefore nestable error -> simplified normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore "{-" "-}" Parser.Nestable |> withPosition)
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = Parser.Expecting "-}"
                          }
                        ]
                    )
    , Test.test "multiCommentBefore nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore "{-" "-}" Parser.Nestable |> withPosition)
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 12 ))
    , Test.test "multiCommentBefore not nestable error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore "{-" "-}" Parser.NotNestable |> withPosition)
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = Parser.Expecting "-}"
                          }
                        ]
                    )
    , Test.test "multiCommentBefore not nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentBefore "{-" "-}" Parser.NotNestable |> withPosition)
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 12 ))
    ]


multiCommentAfterTests : List Test
multiCommentAfterTests =
    [ Test.test "multiCommentAfter nestable error -> simplified normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter "{-" "-}" Parser.Nestable |> withPosition)
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = Parser.Expecting "-}"
                          }
                        ]
                    )
    , Test.test "multiCommentAfter nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter "{-" "-}" Parser.Nestable |> withPosition)
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 14 ))
    , Test.test "multiCommentAfter not nestable error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter "{-" "-}" Parser.NotNestable |> withPosition)
                "{- comment --"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 14
                          , problem = Parser.Expecting "-}"
                          }
                        ]
                    )
    , Test.test "multiCommentAfter not nestable ok -> no error" <|
        \() ->
            Parser.run
                (Workaround.multiCommentAfter "{-" "-}" Parser.NotNestable |> withPosition)
                "{- comment -} rest"
                |> Expect.equal
                    (Ok ( 1, 14 ))
    ]


lineCommentBeforeTests : List Test
lineCommentBeforeTests =
    [ Test.test "lineCommentBefore error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentBefore "--" |> withPosition)
                "{- comment\nrest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = Parser.Expecting "--"
                          }
                        ]
                    )
    , Test.test "lineCommentBefore ok at end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentBefore "--" |> withPosition)
                "-- comment"
                |> Expect.equal
                    (Ok ( 1, 11 ))
    , Test.test "lineCommentBefore ok before end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentBefore "--" |> withPosition)
                "-- comment\nrest"
                |> Expect.equal
                    (Ok ( 1, 11 ))
    ]


lineCommentAfterTests : List Test
lineCommentAfterTests =
    [ Test.test "lineCommentAfter error -> normal error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentAfter "--" |> withPosition)
                "{- comment\nrest"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , problem = Parser.Expecting "--"
                          }
                        ]
                    )
    , Test.test "lineCommentAfter ok at end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentAfter "--" |> withPosition)
                "-- comment"
                |> Expect.equal
                    (Ok ( 1, 11 ))
    , Test.test "lineCommentAfter ok before end -> no error" <|
        \() ->
            Parser.run
                (Workaround.lineCommentAfter "--" |> withPosition)
                "-- comment\nrest"
                |> Expect.equal
                    (Ok ( 2, 1 ))
    ]



-- HELPERS


withPosition : Parser a -> Parser ( Int, Int )
withPosition parser =
    parser |> Parser.andThen (\_ -> Parser.getPosition)
