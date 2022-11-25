import Data.String.Conversions
import Data.Yaml as Y (encode)
import Lib1 (State (..))
import Lib2 (gameStart, hint, renderDocument)
import Lib3 (parseDocument)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Types (Coord (..), Document (..))

main :: IO ()
main =
  defaultMain
    ( testGroup
        "Tests"
        [ toYamlTests,
          fromYamlTests,
          gameStartTests,
          hintTests,
          properties
        ]
    )

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

golden :: TestTree
golden =
  testGroup
    "Handles foreign rendering"
    [ testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
        \doc -> parseDocument (cs (Y.encode doc)) == Right doc
    ]

dogfood :: TestTree
dogfood =
  testGroup
    "Eating your own dogfood"
    [ testProperty "parseDocument (renderDocument doc) == doc" $
        \doc -> parseDocument (renderDocument doc) == Right doc
    ]

fromYamlTests :: TestTree
fromYamlTests =
  testGroup
    "Document from yaml"
    [ testCase "null" $
        parseDocument "---\nnull" @?= Right DNull,
      testCase "single digit number" $
        parseDocument "1" @?= Right (DInteger 1),
      testCase "multi digit number" $
        parseDocument "123" @?= Right (DInteger 123),
      testCase "multi digit number" $
        parseDocument "-456" @?= Right (DInteger (-456)),
      testCase "one word" $
        parseDocument "\"Hello\"" @?= Right (DString "Hello"),
      testCase "multiple words" $
        parseDocument "\"Hello world!\"" @?= Right (DString "Hello world!"),
      testCase "empty list" $
        parseDocument "[]" @?= Right (DList []),
      testCase "list" $
        parseDocument "- null\n- 2\n- Hello\n" @?= Right (DList [DNull, DInteger 2, DString "Hello"]),
      testCase "nested list" $
        parseDocument nestedList @?= Right (DList [DList [DInteger 1, DNull, DString "Hello"]]),
      testCase "list of list" $
        parseDocument listOfList @?= Right (DList [DList [DInteger 1, DNull], DList [DString "Hello", DString "World", DList [DInteger 3, DInteger 4, DInteger 5]]]),
      testCase "empty DMap" $
        parseDocument "{}\n" @?= Right (DMap[]),
      testCase "null in DMap" $
        parseDocument "Nulas: null" @?= Right (DMap[("Nulas", DNull)]),
      testCase "int in DMap" $
        parseDocument "Number: -123" @?= Right (DMap[("Number", DInteger (-123))]),
      testCase "string in DMap" $
        parseDocument "String: sss" @?= Right (DMap[("String", DString "sss")]),
      testCase "multiple simple elements in DMap" $
        parseDocument "Null: null\nNumber: 74679\nWord: \"Hello\"" @?= Right (DMap[("Null", DNull), ("Number", DInteger 74679), ("Word", DString "Hello")]),
      testCase "list in map" $
        parseDocument (renderDocument (DMap[("List", DList[DInteger 1, DInteger 2, DInteger 3]), ("Num", DInteger 5)])) @?= Right (DMap[("List", DList[DInteger 1, DInteger 2, DInteger 3]), ("Num", DInteger 5)]),
      testCase "map in list" $
        parseDocument "- Num: 3\n  Nummm: 5\n" @?= Right (DList[DMap[("Num", DInteger 3), ("Nummm", DInteger 5)]]),
      testCase "map in map" $
        parseDocument "Map:\n  Mapp: null\n" @?= Right (DMap[("Map", DMap[("Mapp", DNull)])]),
      testCase "string with a digit" $
        parseDocument (cs (Y.encode(DString "8KC984"))) @?= Right (DString "8KC984"),
      testCase "fromConsole" $
        parseDocument (cs (Y.encode(DMap [("E",DMap [("t",DList [])])]))) @?= Right (DMap [("E",DMap [("t",DList [])])]),
      testCase "fromConsole" $
        parseDocument (renderDocument (DMap [("E",DMap [("t",DList [])])])) @?= Right (DMap [("E",DMap [("t",DList [])])]),
      testCase "empty in list in a list" $
        parseDocument (renderDocument (DList[DList[]])) @?= Right (DList[DList[]]),
      testCase "empty map in a list" $
        parseDocument (renderDocument (DList[DMap[]])) @?= Right (DList[DMap[]]),
      testCase "empty list in a map" $
        parseDocument (renderDocument (DMap[("Empty", DList[])])) @?= Right (DMap[("Empty", DList[])]),
      testCase "empty map in a map" $
        parseDocument (renderDocument (DMap[("Empty", DMap[])])) @?= Right (DMap[("Empty", DMap[])]),
      testCase "empty in list in a list" $
        parseDocument (cs (Y.encode(DList[DList[]]))) @?= Right (DList[DList[]]),
      testCase "empty map in a list" $
        parseDocument (cs (Y.encode(DList[DMap[]]))) @?= Right (DList[DMap[]]),
      testCase "empty list in a map" $
        parseDocument (cs (Y.encode(DMap [("Empty", DList[])]))) @?= Right (DMap[("Empty", DList[])]),
      testCase "empty map in a map" $
        parseDocument (cs (Y.encode(DMap [("Empty", DMap[])]))) @?= Right (DMap[("Empty", DMap[])]),
      testCase "foreign1" $
        parseDocument (cs (Y.encode(DMap [("lz",DList [DMap [("mG",DList [DMap []]),("hg",DMap [("lS",DString "9"),("V",DList [DInteger 1])])]])]))) @?= Right (DMap [("lz",DList [DMap [("mG",DList [DMap []]),("hg",DMap [("lS",DString "9"),("V",DList [DInteger 1])])]])]),
      testCase "foreign2" $
        parseDocument (cs (Y.encode(DMap [("N",DMap [("A",DMap [("E",DMap [("d",DInteger 0)])])])]))) @?= Right (DMap [("N",DMap [("A",DMap [("E",DMap [("d",DInteger 0)])])])]),
      testCase "dog" $
        parseDocument (renderDocument (DList [DString "",DInteger 0,DList [DMap [("lQz",DInteger 0),("OwiR",DString "J")],DList [DString "y0P7"]]])) @?= Right (DList [DString "",DInteger 0,DList [DMap [("lQz",DInteger 0),("OwiR",DString "J")],DList [DString "y0P7"]]])
      ]

listOfList :: String
listOfList =
  unlines
    [ "---",
      "- - 1",
      "  - null",
      "- - Hello",
      "  - World",
      "  - - 3",
      "    - 4",
      "    - 5"
    ]

nestedList :: String
nestedList =
  unlines
    [ "---",
      "- - 1",
      "  - null",
      "  - Hello"
    ]

toYamlTests :: TestTree
toYamlTests =
  testGroup
    "Document to yaml"
    [ testCase "null" $
        renderDocument DNull @?= "---\nnull",
      testCase "int" $
        renderDocument (DInteger 5) @?= "---\n5",
      testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts,
      testCase "String" $
        renderDocument (DString "String test") @?= "---\n\"String test\"",
      testCase "list of primitives" $
        renderDocument (DList [DInteger 5, DInteger 6, DNull, DString "testing string", DInteger 123, DNull, DString "abc", DString "12345"]) @?= listOfPrimitives,
      testCase "List of lists" $
        renderDocument (DList [DList [DInteger 1, DInteger 2, DInteger 3], DList [], DList [DString "Hello", DNull, DString "Hi"]]) @?= listOfLists,
      testCase "list in list in list in list" $
        renderDocument (DList [DList [DList [DList [DNull]]]]) @?= listInListInListInList,
      testCase "list in list in list in list with numbers" $
        renderDocument (DList [DList [DInteger 4, DInteger 5, DInteger 6, DList [DList [DString "Sveikas", DString "Pasauli"]], DInteger 7]]) @?= listInListInListInListWithNumbers,
      testCase "DMap" $
        renderDocument (DMap [("coords", DList [DMap [("col", DInteger 1), ("row", DInteger 6)], DMap [("col", DInteger 1), ("row", DInteger 9)]])]) @?= coords2,
      testCase "DDMap" $
        renderDocument (DMap [("Test1", DMap [("Test2.1", DMap [("Test3", DNull)]), ("Test2.2", DMap [("Test3", DList [DInteger 7, DInteger 8]), ("Test3", DInteger 4)])])]) @?= dMapTest,
      testCase "TrickySpooky Halloween-Themed" $
        renderDocument trickyCaseDocument @?= trickyCaseString
    ]

trickyCaseDocument :: Document
trickyCaseDocument =
  DMap
    [ ( "key1",
        DMap
          [ ( "key2",
              DList
                [ DInteger 1,
                  DMap
                    [ ( "key3",
                        DList
                          [ DInteger 1,
                            DInteger 3,
                            DNull,
                            DMap [("", DNull)],
                            DMap []
                          ]
                      ),
                      ("key4", DString "")
                    ],
                  DNull
                ]
            )
          ]
      ),
      ("key5", DList [])
    ]

trickyCaseString :: String
trickyCaseString =
  unlines
    [ "---",
      "key1:",
      "  key2:", -- Dėstytojo pavyzdyje "-" prieš DMapo elementus nebuvo dedami
      "  - 1",
      "  - key3:",
      "    - 1",
      "    - 3",
      "    - null",
      "    - null",
      "    - {}",
      "    key4: \"\"",
      "  - null",
      "key5: []"
    ]

dMapTest :: String
dMapTest =
  unlines
    [ "---",
      "Test1:",
      "  Test2.1:",
      "    Test3: null",
      "  Test2.2:",
      "    Test3:",
      "    - 7",
      "    - 8",
      "    Test3: 4"
    ]

coords2 :: String
coords2 =
  unlines
    [ "---",
      "coords:",
      "- col: 1",
      "  row: 6",
      "- col: 1",
      "  row: 9"
    ]

listInListInListInListWithNumbers :: String
listInListInListInListWithNumbers =
  unlines
    [ "---",
      "- - 4",
      "  - 5",
      "  - 6",
      "  - - - \"Sveikas\"",
      "      - \"Pasauli\"",
      "  - 7"
    ]

listInListInListInList :: String
listInListInListInList =
  unlines
    [ "---",
      "- - - - null"
    ]

listOfLists :: String
listOfLists =
  unlines
    [ "---",
      "- - 1",
      "  - 2",
      "  - 3",
      "- []",
      "- - \"Hello\"",
      "  - null",
      "  - \"Hi\""
    ]

listOfPrimitives :: String
listOfPrimitives =
  unlines
    [ "---",
      "- 5",
      "- 6",
      "- null",
      "- \"testing string\"",
      "- 123",
      "- null",
      "- \"abc\"",
      "- \"12345\""
    ]

listOfInts :: String
listOfInts =
  unlines
    [ "---",
      "- 5",
      "- 6"
    ]

gameStartTests :: TestTree
gameStartTests =
  testGroup
    "Test start document"
    [ testCase "game start from server" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc @?= Right (State {hintedCells = [], toggledCells = [], occupiedCols = [2, 0, 2, 2, 2, 0, 6, 0, 3, 3], occupiedRows = [0, 2, 4, 2, 4, 1, 3, 2, 1, 1], noOfHints = 10}),
      testCase "incorrect category string" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc2 @?= Left "could not find \"occupied_cols\"",
      testCase "DMap instead of DList" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc3 @?= Left "invalid DMap [(\"kazkas\",DInteger 5)] argument, expected DList [DInteger...]",
      testCase "DString in DList instead of DInteger in DList" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc4 @?= Left "invalid DList [DString \"labas\"] argument, expected DList [DInteger...]",
      testCase "DNull instead of DInteger" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc5 @?= Left "invalid DNull argument, expected DInteger",
      testCase "DInteger instead of DMap" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) (DInteger 5) @?= Left "invalid DInteger 5 argument, expected DMap"
    ]

serverDoc :: Document
serverDoc = DMap [("number_of_hints", DInteger 10), ("occupied_cols", DList [DInteger 2, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 6, DInteger 0, DInteger 3, DInteger 3]), ("occupied_rows", DList [DInteger 0, DInteger 2, DInteger 4, DInteger 2, DInteger 4, DInteger 1, DInteger 3, DInteger 2, DInteger 1, DInteger 1])]

serverDoc2 :: Document
serverDoc2 = DMap [("number_of_hints", DInteger 10), ("occupied_colss", DList [DInteger 2, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 6, DInteger 0, DInteger 3, DInteger 3]), ("occupied_rows", DList [DInteger 0, DInteger 2, DInteger 4, DInteger 2, DInteger 4, DInteger 1, DInteger 3, DInteger 2, DInteger 1, DInteger 1])]

serverDoc3 :: Document
serverDoc3 = DMap [("number_of_hints", DInteger 10), ("occupied_cols", DMap [("kazkas", DInteger 5)]), ("occupied_rows", DList [DInteger 0, DInteger 2, DInteger 4, DInteger 2, DInteger 4, DInteger 1, DInteger 3, DInteger 2, DInteger 1, DInteger 1])]

serverDoc4 :: Document
serverDoc4 = DMap [("number_of_hints", DInteger 10), ("occupied_cols", DList [DString "labas"]), ("occupied_rows", DList [DInteger 0, DInteger 2, DInteger 4, DInteger 2, DInteger 4, DInteger 1, DInteger 3, DInteger 2, DInteger 1, DInteger 1])]

serverDoc5 :: Document
serverDoc5 = DMap [("number_of_hints", DNull), ("occupied_cols", DList [DInteger 2, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 6, DInteger 0, DInteger 3, DInteger 3]), ("occupied_rows", DList [DInteger 0, DInteger 2, DInteger 4, DInteger 2, DInteger 4, DInteger 1, DInteger 3, DInteger 2, DInteger 1, DInteger 1])]

hintTests :: TestTree
hintTests =
  testGroup
    "Test hint document"
    [ testCase "DInteger instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) (DInteger 5) @?= Left "invalid DInteger 5 argument, expected DMap",
      testCase "DString instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) (DString "labas") @?= Left "invalid DString \"labas\" argument, expected DMap",
      testCase "DList instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) (DList [DInteger 5, DInteger 0]) @?= Left "invalid DList [DInteger 5,DInteger 0] argument, expected DMap",
      testCase "correct coordinates" $
        hint (State {hintedCells = [Coord {col = 6, row = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) doc @?= Right (State {hintedCells = [Coord {col = 6, row = 1}, Coord {col = 6, row = 2}, Coord {col = 6, row = 3}, Coord {col = 6, row = 4}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}),
      testCase "DNull instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) doc2 @?= Left "invalid DNull argument, expected DInteger",
      testCase "DString instead of DInteger" $
        hint (State {hintedCells = [Coord {col = 6, row = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) doc3 @?= Left "invalid DString \"kazkas\" argument, expected DInteger"
    ]

doc :: Document
doc = DMap [("coords", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 1)]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 2)]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 3)]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 4)]), ("tail", DNull)])])])])]

doc2 :: Document
doc2 = DMap [("coords", DMap [("head", DMap [("col", DNull), ("row", DNull)]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 2)]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 3)]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 4)]), ("tail", DNull)])])])])]

doc3 :: Document
doc3 = DMap [("coords", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 1)]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 2)]), ("tail", DMap [("head", DMap [("col", DString "kazkas"), ("row", DString "abc")]), ("tail", DMap [("head", DMap [("col", DInteger 6), ("row", DInteger 4)]), ("tail", DNull)])])])])]