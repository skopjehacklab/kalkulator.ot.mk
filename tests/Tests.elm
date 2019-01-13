module Tests exposing (dummyTest)

import Expect
import Test exposing (..)


dummyTest : Test
dummyTest =
    test "dummy test" <|
        \_ ->
            Expect.equal 1 1
