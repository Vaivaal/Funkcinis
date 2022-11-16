import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encode )

import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..), Coord(..))
import Lib1 (State(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (cs (Y.encode doc)) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument "null" @?= Right DNull
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "---\nnull"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "---\n5"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "String" $
        renderDocument (DString "String test") @?= "---\nString test"
    , testCase "list of primitives" $
        renderDocument (DList [DInteger 5, DInteger 6, DNull, DString "testing string", DInteger 123, DNull, DString "abc", DString "12345"]) @?= listOfPrimitives
    , testCase "List of lists" $
        renderDocument (DList [DList[DInteger 1, DInteger 2, DInteger 3], DList[], DList[DString "Hello", DNull, DString "Hi"]]) @?= listOfLists
    , testCase "list in list in list in list" $
        renderDocument (DList [DList [DList [DList [DNull]]]]) @?= listInListInListInList
    , testCase "list in list in list in list with numbers" $
        renderDocument (DList [DList [DInteger 4, DInteger 5, DInteger 6, DList [DList [DString "Sveikas", DString "Pasauli"]], DInteger 7]]) @?= listInListInListInListWithNumbers
    , testCase "DMap" $
        renderDocument (DMap [("coords", DList [DMap [("col", DInteger 1), ("row", DInteger 6)], DMap [("col", DInteger 1), ("row", DInteger 9)]])]) @?= coords2
    , testCase "DDMap" $
        renderDocument (DMap [("Test1", DMap [("Test2.1", DMap [("Test3", DNull)]), ("Test2.2", DMap [("Test3", DList[DInteger 7, DInteger 8]), ("Test3", DInteger 4)])])]) @?= dMapTest
    , testCase "TrickySpooky Halloween-Themed" $
        renderDocument trickyCaseDocument @?= trickyCaseString
        -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

trickyCaseDocument :: Document
trickyCaseDocument =
 DMap [
    ("key1", DMap [
        ("key2", DList [
            DInteger 1,
            DMap [
                ("key3", DList [
                    DInteger 1,
                    DInteger 3,
                    DNull,
                    DMap [("", DNull)],
                    DMap []
                ]),
                ("key4", DString "")],
            DNull
        ])
    ]),
    ("key5", DList [])
 ]

trickyCaseString :: String
trickyCaseString = unlines [
     "---",
     "key1:",
     "  key2:", -- Dėstytojo pavyzdyje "-" prieš DMapo elementus nebuvo dedami
     "  - 1",
     "  - key3:",
     "    - 1",
     "    - 3",
     "    - null",
     "    - null",
     "    - []",
     "    key4: \"\"",
     "  - null",
     "key5: []"
 ]

dMapTest :: String
dMapTest = unlines [
    "---"
  , "Test1:"
  , "  Test2.1:"
  , "    Test3: null"
  , "  Test2.2:"
  , "    Test3:"
  , "    - 7"
  , "    - 8"
  , "    Test3: 4"
  ]

coords2 :: String
coords2 = unlines [
      "---"
    , "coords:"
    , "- col: 1"
    , "  row: 6"
    , "- col: 1"
    , "  row: 9"
  ]

listInListInListInListWithNumbers :: String
listInListInListInListWithNumbers = unlines [
      "---"
    , "- - 4"
    , "  - 5"
    , "  - 6"
    , "  - - - \"Sveikas\""
    , "      - \"Pasauli\""
    , "  - 7"
  ]

listInListInListInList :: String
listInListInListInList = unlines [
      "---"
    , "- - - - null"
  ]

listOfLists :: String
listOfLists = unlines [
      "---"
    , "- - 1"
    , "  - 2"
    , "  - 3"
    , "- []"
    , "- - \"Hello\""
    , "  - null"
    , "  - \"Hi\""
  ]

listOfPrimitives :: String
listOfPrimitives = unlines [
      "---"
    , "- 5"
    , "- 6"
    , "- null"
    , "- \"testing string\""
    , "- 123"
    , "- null"
    , "- \"abc\""
    , "- \"12345\""
  ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" 
  [   testCase "game start from server" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc @?= Right (State {hintedCells = [], toggledCells = [], occupiedCols = [2,0,2,2,2,0,6,0,3,3], occupiedRows = [0,2,4,2,4,1,3,2,1,1], noOfHints = 10})
    , testCase "incorrect category string" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc2 @?= Left "could not find \"occupied_cols\""
    , testCase "DMap instead of DList" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc3 @?= Left "invalid DMap [(\"kazkas\",DInteger 5)] argument, expected DList [DInteger...]"
    , testCase "DString in DList instead of DInteger in DList" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc4 @?= Left "invalid DList [DString \"labas\"] argument, expected DList [DInteger...]"
    , testCase "DNull instead of DInteger" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) serverDoc5 @?= Left "invalid DNull argument, expected DInteger"
    , testCase "DInteger instead of DMap" $
        gameStart (State {hintedCells = [], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 0}) (DInteger 5) @?= Left "invalid DInteger 5 argument, expected DMap"
    
  ]

serverDoc :: Document
serverDoc = DMap [("number_of_hints",DInteger 10),("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),("occupied_rows",DList [DInteger 0,DInteger 2,DInteger 4,DInteger 2,DInteger 4,DInteger 1,DInteger 3,DInteger 2,DInteger 1,DInteger 1])]

serverDoc2 :: Document
serverDoc2 = DMap [("number_of_hints",DInteger 10),("occupied_colss",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),("occupied_rows",DList [DInteger 0,DInteger 2,DInteger 4,DInteger 2,DInteger 4,DInteger 1,DInteger 3,DInteger 2,DInteger 1,DInteger 1])]

serverDoc3 :: Document
serverDoc3 = DMap [("number_of_hints",DInteger 10),("occupied_cols",DMap [("kazkas", DInteger 5)]),("occupied_rows",DList [DInteger 0,DInteger 2,DInteger 4,DInteger 2,DInteger 4,DInteger 1,DInteger 3,DInteger 2,DInteger 1,DInteger 1])]

serverDoc4 :: Document
serverDoc4 = DMap [("number_of_hints",DInteger 10),("occupied_cols",DList [DString "labas"]),("occupied_rows",DList [DInteger 0,DInteger 2,DInteger 4,DInteger 2,DInteger 4,DInteger 1,DInteger 3,DInteger 2,DInteger 1,DInteger 1])]

serverDoc5 :: Document
serverDoc5 = DMap [("number_of_hints",DNull),("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),("occupied_rows",DList [DInteger 0,DInteger 2,DInteger 4,DInteger 2,DInteger 4,DInteger 1,DInteger 3,DInteger 2,DInteger 1,DInteger 1])]

hintTests :: TestTree
hintTests = testGroup "Test hint document" 
  [   testCase "DInteger instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row  = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) (DInteger 5) @?= Left "invalid DInteger 5 argument, expected DMap"
    , testCase "DString instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row  = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) (DString "labas") @?= Left "invalid DString \"labas\" argument, expected DMap"
    , testCase "DList instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row  = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) (DList [DInteger 5, DInteger 0]) @?= Left "invalid DList [DInteger 5,DInteger 0] argument, expected DMap"
    , testCase "correct coordinates" $
        hint (State {hintedCells = [Coord {col = 6, row  = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) doc @?= Right (State {hintedCells = [Coord {col = 6, row = 1},Coord {col = 6, row = 2},Coord {col = 6, row = 3},Coord {col = 6, row = 4}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6})
    , testCase "DNull instead of DMap" $
        hint (State {hintedCells = [Coord {col = 6, row  = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) doc2 @?= Left "invalid DNull argument, expected DInteger"
    , testCase "DString instead of DInteger" $
        hint (State {hintedCells = [Coord {col = 6, row  = 1}], toggledCells = [], occupiedCols = [], occupiedRows = [], noOfHints = 6}) doc3 @?= Left "invalid DString \"kazkas\" argument, expected DInteger"
  ]

doc :: Document 
doc = DMap [("coords",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 1)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 2)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 3)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 4)]),("tail",DNull)])])])])]

doc2 :: Document
doc2 = DMap [("coords",DMap [("head",DMap [("col",DNull),("row",DNull)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 2)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 3)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 4)]),("tail",DNull)])])])])]

doc3 :: Document
doc3 = DMap [("coords",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 1)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 2)]),("tail",DMap [("head",DMap [("col",DString "kazkas"),("row",DString "abc")]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 4)]),("tail",DNull)])])])])]