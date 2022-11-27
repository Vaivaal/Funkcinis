import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)
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

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden =
  testGroup
    "Handles foreign rendering"
    [ testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
    \doc -> parseDocument (friendlyEncode doc) == Right doc
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
        parseDocument "00123" @?= Right (DInteger 123),
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
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DString "8KC984"))) @?= Right (DString "8KC984"),
      testCase "fromConsole" $
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DMap [("E",DMap [("t",DList [])])]))) @?= Right (DMap [("E",DMap [("t",DList [])])]),
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
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DList[DList[]]))) @?= Right (DList[DList[]]),
      testCase "empty map in a list" $
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DList[DMap[]]))) @?= Right (DList[DMap[]]),
      testCase "empty list in a map" $
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DMap [("Empty", DList[])]))) @?= Right (DMap[("Empty", DList[])]),
      testCase "empty map in a map" $
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DMap [("Empty", DMap[])]))) @?= Right (DMap[("Empty", DMap[])]),
      testCase "foreign1" $
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DMap [("lz",DList [DMap [("mG",DList [DMap []]),("hg",DMap [("lS",DString "9"),("V",DList [DInteger 1])])]])]))) @?= Right (DMap [("lz",DList [DMap [("mG",DList [DMap []]),("hg",DMap [("lS",DString "9"),("V",DList [DInteger 1])])]])]),
      testCase "foreign2" $
        parseDocument (cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions)(DMap [("N",DMap [("A",DMap [("E",DMap [("d",DInteger 0)])])])]))) @?= Right (DMap [("N",DMap [("A",DMap [("E",DMap [("d",DInteger 0)])])])]),
      testCase "dog" $
        parseDocument (renderDocument (DList [DString "",DInteger 0,DList [DMap [("lQz",DInteger 0),("OwiR",DString "J")],DList [DString "y0P7"]]])) @?= Right (DList [DString "",DInteger 0,DList [DMap [("lQz",DInteger 0),("OwiR",DString "J")],DList [DString "y0P7"]]]),
      testCase "mail" $
        parseDocument (renderDocument fromMail) @?= Right fromMail
      ]

fromMail :: Document
fromMail = DMap [("SYTQtb",DMap [("XvCZrAhfw",DList [DMap [("ZB",DMap []),("usndsb",DList [DInteger 87,DString "aU",DInteger (-51)]),("JwZmDkT",DString "z")],DInteger (-49),DMap [("oXRHkJopcp",DString "R"),("GxbryTKA",DInteger 7),("U",DList [DMap [("ZqWeisxt",DList [DMap [("uMA",DList [DList [DInteger 58,DInteger (-7),DList [DString "3I159 8xdd2",DString "VF F6i",DMap [("O",DInteger (-72)),("bHo",DString "Vj1 d rQ8 4"),("ryZezNvpKI",DString "Et 56 B d2r  1"),("XQtffWSJ",DList [])],DInteger 93],DMap [("zRUBB",DInteger (-66)),("EBFczmwBq",DMap [("dFYi",DList [DMap [("sKkNWqUbL",DInteger (-44)),("o",DInteger (-20)),("o",DInteger (-11)),("TNfDT",DList [DList []])]]),("kfjnDJ",DList [DMap [("pxjcA",DString " 0IZt "),("viqgpfBx",DInteger 65),("slDFN",DMap [("z",DInteger 80),("mFVXh",DString "z0"),("SW",DString "363 1 S 917q9 pA"),("QWeGoLB",DMap [("ThU",DString "1   ")])]),("nqOVWnEnI",DMap [("FJ",DList [DMap [("PzrQaIlVwa",DMap [("hCXRR",DMap [("YjK",DString "9vlX5 Ci"),("pdseLD",DMap [("QBnYyshqt",DInteger (-76))]),("IQUAQLfdSC",DString "R344 w 43yDOt4 T"),("cQCyoHdOnR",DString " g  u xJ")]),("lzk",DMap [("X",DMap [("xzVMGRykL",DString "8ZtB "),("AIvlu",DInteger (-49))])])])],DInteger (-35),DInteger 87]),("WMVBATf",DString "91 HSPi9Idvtaf3M")])],DString " A 99A ciKrX 8F",DMap [("Z",DString " v6 z9 21 ptI m"),("OIbwpjKB",DString "n"),("r",DMap [("iJk",DString "0bu"),("IJv",DInteger (-60)),("epPW",DString "T7 IuJ   6")]),("ezTaI",DList [DString "nK Wh",DInteger (-26),DMap [("eThjk",DMap [("p",DString "8E"),("rwy",DInteger 22)]),("Fc",DString "ls882I RI  ak")],DMap [("moA",DList [DString "h9H0Li3 zu3",DList [DMap [("lPzXhaJI",DString "W"),("P",DList [DMap [("Z",DString "Z p36Z "),("GGUOx",DList [DInteger 34]),("ndOrU",DMap [("xwEX",DString "S"),("OVsGC",DInteger 25),("QDKwmt",DString "")]),("Zx",DString " FrT 3iRdbgZ2")],DInteger (-72),DList [],DList [DMap [("q",DList [DInteger (-98),DList [DString "x61",DString "GT  W4"]])],DList [DString "YIq LyQJ ",DInteger 45,DInteger 42,DInteger (-69)]]])],DList [DList [DList [DMap [("gGKtVdF",DInteger (-96)),("HkMLPa",DList [DInteger 23,DList [DList [DInteger 22,DList [DString " Fd03AA",DList [DString "b",DMap [("eydzobhl",DList [DInteger (-97)]),("XTiC",DInteger 62),("rI",DList [DMap [("Y",DMap [("vbwntZa",DList []),("SuRjts",DMap [("Wloejirw",DInteger (-8)),("kjVUK",DInteger (-79))]),("wUuWR",DInteger (-34)),("lANmEtZqBF",DMap [("XPR",DMap [("LWuLQhXKE",DInteger 29),("viBVwv",DList [DString "t",DList [DList [DList [DMap [("mpE",DList [DInteger (-79),DString "6M A U33ZaA V",DString "0xg  tX",DInteger 42]),("fdWljHHr",DString "3o9 8  8 T f1")]]],DInteger (-11)],DMap [("mhs",DMap [("vQvWk",DMap [("dGhzshpx",DMap [("HVEOI",DInteger (-92)),("EoVnaXCT",DMap [])]),("EezQFTTbtS",DInteger 13),("lal",DString "D  ojL354   3 76")]),("TMhjOQ",DList [DString "R  6H 437",DMap [("ySzTVoNf",DList [DInteger (-33),DMap [("f",DInteger 69),("PlZbgaks",DMap []),("ATzeXCB",DInteger 12),("Yv",DString "2    9")],DString "9o MI I xTSuf q"]),("FWahtY",DInteger 78),("Sk",DList [DList [DList [DInteger 63],DList [DInteger 3]],DInteger (-76)]),("aHLRjUqXzN",DInteger 91)]])]),("L",DMap [("K",DList [DMap [("MFfyLQCIg",DString "Q")],DList [DInteger (-84),DString "86  t 9F0 zo Yw"],DString " 3 I yb  F1p ",DInteger 65])]),("v",DString " ")]])]),("wBYZdOxR",DMap []),("IhLMpI",DMap [("ZOH",DInteger 26),("hncEpyICq",DInteger 84)]),("pxHnwKi",DList [DString "12icm  1R78 Zl"])])]),("G",DString "uPltW X4T"),("sFcunRHcD",DMap [("swV",DString "v6MDD5pLA ")])],DList [DMap [("VaEHwA",DString "epW jKSd "),("CryKzZ",DList [DInteger (-48),DMap [("DZHDCcQ",DList [DList [DString "gX v x7a L EzR"]])]])]],DInteger (-10)]),("SnKbUetTVG",DInteger (-65))],DString "    p",DString "f0   0Q2 k "]]],DMap [("JxXTa",DInteger 52)],DString "jg6 oaQk2WDzpN",DList [DList [DString "aFDeSue ",DInteger (-91)],DMap [("h",DString "N2Z2 3 z5 8 A"),("xvIrzDjI",DMap [("aZSqK",DMap []),("vOR",DString "S 3 l O "),("bB",DMap [("pPIpk",DList [DMap [],DInteger 73,DList [DMap [],DMap [("MzPYrKX",DInteger 9),("EFEJWx",DList [DList [],DString " rSd3 "]),("aiNzMR",DList [DInteger (-14),DMap [("NrO",DInteger (-85))]])]]]),("D",DMap [("btTx",DList [DMap [],DString " x rOL0 6"]),("gzAUmRuKOE",DString "gu0gyL46 9 a3"),("Kv",DString "Pv ")]),("hADvebIyG",DInteger 14)])]),("fJF",DInteger (-94))],DList [DInteger (-70)],DString " S35  H  atU"]],DString " YA0a7SK74"])],DList [DInteger (-99),DString "5 7  Q8"]],DList [DList [DString " A32",DList [DString "Eh1w"],DList [DList [DList [DString "X"],DMap [("BjXegPPejy",DString "qezy  r4k k"),("iLYMZ",DString "Ck   QB44W19  P"),("CmMOhxWcG",DMap [("T",DMap [("HdmfQlor",DString "2y72QX O2OD8  8M")]),("NrPwuJ",DString "80 49 2O900l "),("viwt",DList [DMap [("uj",DMap [("ueDDIIH",DList [DInteger 93])]),("GJbxbph",DInteger (-56)),("FBGa",DString "RlBfv8a75")],DInteger (-75),DList [DInteger 79,DList [DMap []],DInteger (-63)],DString "V6 m X v a 8"]),("oBizkvP",DList [DString " wRY 65 H66Wgt",DInteger 62,DString "Ql1 UVs9Nm3r",DMap [("iC",DInteger 26),("kq",DMap [("xTxjOJzJh",DMap [("dS",DList [DInteger (-6),DMap [("XvGO",DMap [("gwilL",DInteger 50),("LGSdoy",DMap [("I",DList [DInteger 31]),("wPycwW",DMap [("wUzoX",DMap [("boBsG",DMap [("B",DMap [("Tfu",DInteger 0)]),("I",DInteger 52),("r",DInteger 30)])]),("qCxStahrSs",DList [])]),("GqkU",DString " J0 J9")])])],DString "P15p hPt5"]),("mdsFkMgOIX",DString "2 b79 gkYgz48"),("jgnirXAowh",DList [])]),("BkZONL",DString "ZB8VDvCs  p"),("liSrM",DList [])]),("PKr",DMap [("y",DString " 9sW5btgc "),("Ao",DMap [("ZIZSgl",DList [DString "e0VW H4Bj",DMap [("bzKjScK",DString "xu053U KO iE"),("aVGXaBWrG",DMap []),("C",DString "P 4KqfY 3fwoAP6a"),("FZ",DString "2D B3 14ry1")],DList [DInteger 13,DString "xAU0",DMap [("ZT",DString " A616")],DString "2 p gn w31VSsR J"]]),("cLDF",DString ""),("pIaMUhou",DList [DMap [("wx",DMap []),("DfNXSgT",DInteger 42),("vRr",DMap [("XcursAvQX",DInteger (-82)),("eRNNwwELr",DString "97X0 2978 ")]),("ANB",DMap [])],DMap [("MrsggSv",DString "  H25 83 0  b")],DInteger (-80),DString " X5  hH"])]),("QJycpj",DList [DList [DMap [("Ze",DMap [("xLjEW",DString "yfSU0"),("eCh",DMap [("WDhXpLIGJl",DString " wL  eadcq"),("nqLK",DMap [("WehoGMwHj",DMap [("cdhYMkwN",DMap []),("ndMZ",DList [DString " 442  3 v ac4T",DInteger 84,DList []]),("wp",DInteger (-20)),("FThKAA",DString "")]),("rM",DList []),("JIjZY",DMap [("DvjkQ",DList [DMap [("K",DInteger 95)],DMap [("EH",DInteger (-77)),("uvUoBwsTu",DInteger 8)],DInteger 19,DInteger (-15)]),("gCVT",DList []),("AhzwQi",DList [DMap [("Iyltjsz",DMap []),("NKv",DString " 4eOb we 3Rw")],DMap [("YUmAG",DList [])],DString "nd ",DInteger (-94)]),("aNop",DList [DMap [],DList [DMap [("rnROfsO",DInteger (-53)),("QmeoKtPu",DList []),("HENOcLa",DMap [("ypy",DMap [("ITL",DList [DMap [("n",DList []),("RZKtuVICG",DMap [("u",DInteger 37),("zcZcLE",DMap [("wIkFxD",DString "z0")])])]])]),("CXcJnGso",DInteger 52),("lrfMrt",DMap [])]),("oGHQeMpe",DString " 1a23B")],DMap [("rwyVEvxCB",DMap [("Nn",DString "It7"),("viUuHDmjSx",DInteger 70),("TNO",DMap [("pMOdwfq",DInteger (-14)),("NwbYYg",DString "5 ")])])]]])])])]),("NSBNv",DMap [])])]],DString "j8J0b5  ",DInteger (-74),DInteger (-7)])]),("ghqtcWlm",DMap [("IBU",DMap [("tjkKAoXi",DList [DInteger (-30)]),("KTFVoD",DString "39e  uyJvZ8   s")]),("x",DMap [("pprAVtOnkC",DInteger (-54)),("CNoZdWAPI",DInteger 39),("vySIDlCfi",DMap [("uQMsaZQcAq",DString "zSOz3ha"),("j",DList [DList [DList [DMap [("cA",DList [DList [DInteger 97,DInteger (-50),DList [DList [DInteger 78,DInteger 73,DList [DMap [("y",DInteger (-57)),("IlJFv",DMap [("bEWXOsts",DString "")]),("lk",DList []),("rEBLxc",DInteger 44)],DList [DInteger (-18),DInteger 68,DList []],DInteger 59],DList [DInteger 87,DMap [],DInteger 38,DString "P"]],DString "o  I 4p5d2"],DInteger 47],DString " wkujL1Z3wxjn2"]),("PM",DString " 17   "),("IkZJE",DList [DMap [],DString "tQw gF24TEz9IT"]),("BovXR",DList [DInteger (-73),DList [DInteger (-73),DList [DMap [("OODglEs",DString "   "),("vSGnxjoTu",DString "YS Ju3 OA2ZVc5"),("dYA",DList [DMap [("gR",DList [])],DList [DInteger (-62),DMap [("niNFmyfeeK",DMap [("l",DString "V6K0")]),("VdWgrVHeV",DString "  H 81 "),("pb",DInteger (-65))],DMap [("faOKKZ",DInteger 26)]]]),("qdkAJnvGny",DMap [("xuX",DString " J Ql "),("S",DList [DInteger (-36),DList [DMap [("oG",DString "57 Tg0 0 "),("aMAOamiG",DInteger 68)]],DList [DInteger (-71),DString "  Ib4pt9wK"]]),("Qcqo",DString "3Z ew43krJ69w "),("opPRDPORac",DList [DMap [],DString "j"])])],DString "",DMap [("k",DInteger 81),("GXKNfBg",DList [DInteger 33]),("AjuCCWptoK",DList [DMap [("F",DString " p3  0Wy V bK"),("ChDQl",DInteger (-59)),("KMnjaifX",DMap [("dcTqcEJ",DMap [("RhWqYbJMZ",DInteger 35),("fkpkO",DList [DString " 0Ge5 T0"]),("g",DInteger (-13)),("nu",DString "t P HSl")]),("GcUj",DMap [("a",DInteger (-51)),("gYLP",DInteger 73)]),("o",DInteger 16)])],DInteger 55,DList []])]]]])],DString " "]],DMap [("nIkE",DString "T"),("rFZsYL",DList []),("QPfMP",DList [DList [DInteger (-84),DMap [("CpEuCCRq",DInteger (-21)),("vqShpDbs",DString ""),("UoReaGFJ",DMap [("MNHchopOh",DList [DString "",DMap [],DInteger 96,DString "B 07mh"]),("GcFfoW",DList [DInteger (-74),DString "2",DList [DString "f4I94 ",DList [DMap [("vSunKRRyd",DString "L6y1p77B"),("nI",DMap [("VqKgfuPEvn",DList [DInteger 19,DString "sJ Fs9 Yglbv",DMap [("OJM",DList [DString "  418F hWOok"]),("lIn",DList [DList [],DString "Wl",DInteger (-93),DInteger (-98)]),("AtABY",DString " 0 f NL 0qna0 "),("a",DMap [])]]),("cVxrfxvk",DMap [("VeZYe",DInteger 14)]),("Wb",DMap []),("qFIc",DInteger (-44))]),("xnxwZ",DMap [("NhJBJbf",DInteger (-52)),("pyTYkla",DMap [("ElDDYkixLB",DString "Y LMj 0 8 3D "),("DlqPrH",DInteger (-32))]),("TkKJhSQa",DString "m ")])],DList [DList [DInteger (-79),DInteger 38,DInteger 37],DInteger (-85)],DMap [("qudSPpC",DList [DInteger 98,DInteger 29,DMap [("UJXh",DMap [])],DMap []]),("MPP",DInteger 7)]]]])])],DInteger (-46),DList [DString "wB  sy",DList [],DMap [],DInteger 94]],DString "OG1C0xej B"])]])])]),("bdHAZdbyy",DInteger 55),("aRRZaq",DString "1 e9")])]])])],DString " 7 U  "],DInteger 97]],DString "8x6W2AuSyhh1"]],DInteger 27],DInteger 90,DInteger 27]]),("vOR",DList []),("ZiFNgfLOn",DMap [("DFm",DString "U8o k3AS ")])]])],DList [DList [DList [],DList [],DMap [("LjKO",DList [DInteger (-29),DMap [("kAFVRg",DInteger 83),("c",DMap []),("iyRz",DInteger (-24))]])],DMap [("lQGhiEWSl",DList [DMap []]),("QUEzw",DList []),("ouX",DMap [("KedCHv",DList []),("pHCsckuT",DString "Dr o 4"),("oBLEjjDi",DMap [("gCptCxvbkJ",DMap [("GqzkdpdjZb",DList [DInteger (-69),DList [DString "5   A ZoHU",DInteger (-35)]]),("fGqnstJ",DInteger (-8)),("uc",DInteger 28),("VcPHid",DInteger 50)]),("d",DInteger 41),("acYe",DInteger 2)])]),("V",DList [])]],DList [DList [DInteger 18,DString "UR"],DList [DInteger 3]],DString "R7  2cx"]]),("fjljolLx",DList [DInteger (-19),DInteger (-92),DList [DInteger (-5),DMap [("paR",DString "RCO C U qES32"),("boDvOSC",DInteger (-57))],DString "8 ",DMap []]])]),("EmDjds",DList [DMap [("anjyCGNFel",DInteger 35),("yNDvqcDeh",DString "N Wsf   OoKFXL"),("DuS",DString "V3wS37VB"),("yOkZWlhNUl",DString "u")],DList [DList [DString "b5",DList [DList [DInteger 42,DInteger 70,DList []]],DInteger (-36),DList [DList [DMap [("epLhm",DList [DMap [("dDiWolgv",DList []),("pIyFgD",DList [DString "dul0eEcS  ",DList [DMap [],DMap [("jNRJAVfZH",DInteger 93),("VgLG",DMap [("QVqWnEk",DInteger 65),("ofjc",DInteger 65),("XgPy",DString "6d7I 3")]),("bwHKaoESyc",DList []),("ICnQcR",DList [DMap [],DMap [("CjfmhekaGc",DInteger (-12)),("d",DString "3 C   N2m")]])]],DString "ye Z   s  QN D",DMap [("wimlxcSd",DString "E6KM O9"),("XZp",DMap [("qQOJ",DInteger (-40)),("zvdewh",DInteger (-93))]),("Qdh",DList [DList [DMap [("LHrikQY",DString "b7r D"),("MNS",DMap [("iSq",DList [DList [DMap [("ykr",DInteger 93),("qnOpw",DMap [("UUWgaEY",DMap [("J",DString "NCmv XKw0"),("HCzNccrw",DInteger (-69)),("oeqSaX",DString "07oW8S  "),("gRJFwmm",DMap [("CXtmO",DString "u"),("xwNgOwNve",DMap [("RMbCajubG",DList [DString "o8o4LV4s06mI392C",DMap [("K",DInteger (-75)),("JpnKwXKpz",DInteger (-76)),("mIWfi",DMap [("klHR",DString " 8"),("Ghjhxx",DString "RSQ7 5 vJ g0Fqs"),("obSiymMR",DMap [("CQXB",DInteger (-63))])]),("kPwHxZ",DMap [])],DInteger (-20)]),("pDm",DInteger (-23)),("WAWqBH",DString "6lt 15s")]),("syaDlBNB",DList [DList [DInteger (-99),DInteger 89]])])]),("OErsI",DMap [("qwz",DList [DMap [("rneer",DInteger (-46)),("JHmYdGwVY",DInteger 37),("yrBoZ",DString "gH6q B Z")],DMap [("ATuJbq",DList []),("FjXzirPIw",DInteger 41)]]),("TgqcpR",DInteger 63)]),("SIvyooiDOp",DMap [])]),("LjhI",DMap [("RZqQcG",DString "H387k36Z"),("JSNNENfHn",DString "0Y  eo1")])]],DInteger (-94),DList [DString "5 97L4R4u3 vrD9",DInteger (-71)]]),("RqaCv",DString "4y3 B 8 it9"),("iDcVDi",DInteger (-26))]),("LyIRelvuqy",DString "1z  A8E9    "),("NiKmMKN",DMap [("RrZjAh",DString "qN"),("pwGc",DInteger (-92)),("CCN",DInteger 34),("ftcMShQCg",DString " P")])]],DInteger (-13),DList [DMap [("V",DList [DList [DList [DList [DInteger (-72),DString "SaJUJ 0"],DMap [("EmkjzI",DList [DString " 3  3W Q Q",DMap [],DInteger (-20)]),("gkS",DMap [("rcvgCYGL",DInteger (-44)),("pl",DString "mTt9 Qd"),("BwytQ",DString "cS91oGuX  "),("ljLWhsIfIv",DMap [("C",DList [DString "",DMap [("xiB",DInteger 44),("TcBvdD",DList [])],DInteger (-41)]),("cbEVAU",DString "8"),("bjfGyHYCN",DString "7NtsX I h29D5"),("spAcazEWWo",DString "53Ywz7TvK")])])],DString "0 "],DList [DString "Hi o28P6KVaFV",DList [],DInteger (-80),DString "X pROOG T 48"],DInteger (-84)],DString "0h X TYB cwW8D",DInteger (-77),DMap []]),("tfMQmX",DString "XJ Z 5QB884"),("Hlhoul",DInteger (-47)),("qgpWHmj",DString " 2P g2Ydr 8p  ")],DInteger (-76)],DMap [("SMxqHd",DInteger (-6)),("KquCvebrW",DMap [("v",DString "hVd5H9Sj7J")]),("JwkwV",DList [DMap [("tTfW",DInteger 69)]])]]),("OSG",DInteger 20)]])],DString "v"]),("BCibLUOM",DMap [("kKTqumMxIz",DString "1egpu2"),("inWGaPB",DList []),("zMgSNzswjH",DString "4  h  Ep43 08 n")])]],DList [DString "p7C1X920 l8PLxz3",DString "D 6D",DMap [("tS",DString "1QW 9 6Z"),("erxidWcjDQ",DInteger 9),("n",DInteger (-98))]]]],DList []]])]],DString "2hv5m6Q4Ut2A1s r",DString "5EI1S WyK",DMap [("geT",DList [DMap [("Z",DString "5w uAR"),("tXBuspCh",DList [DMap [("tNva",DList [DMap [],DInteger (-52),DString "0t"]),("cC",DString "ol w d r15h "),("wXDFZlkoX",DInteger 98),("BPIdME",DInteger (-72))],DInteger (-25),DMap [("M",DInteger 58),("HqZOwpw",DString "7f Ms21 D3"),("f",DMap [("Hbks",DList [DList [],DList []]),("ltyu",DMap [("tp",DInteger 67),("PIizgkaEv",DInteger (-17)),("t",DList [DInteger 7,DList [DMap [],DString "cVix0"],DInteger (-65)]),("Kd",DString "f D xP 8W")]),("HlmanIIe",DInteger (-99))])]])],DList [],DList [DMap [("wFXPc",DString "q2m00s  5")],DString "7 0 4 r3e n 2 g "]]),("hQYNBI",DString "uXwO3U  Q")]])],DString " 1jyVr",DList [DInteger 72,DList [DInteger (-97),DList [DInteger (-37)],DList [DList [DInteger 93,DString " 6 3d xI4",DInteger 70],DMap [],DString "U4KEU  "]],DString "on",DList [DString " Q",DMap [("zgSm",DString " N")],DInteger 94,DString "4c"]],DInteger 40]),("MyUxOzaMEW",DMap [("YWJTbHY",DInteger (-26))]),("FbuwbQ",DString "Fd")],DInteger 4,DList [DList [DInteger (-86),DMap [("sO",DString "  bhswCOg19A "),("AvH",DString "tGLkt 0Z"),("CP",DInteger (-1)),("tEWSDXYU",DList [DInteger (-31),DList [DMap [("Hv",DMap [("iZHyVtNB",DMap [("QZCGJfJ",DList [DMap [],DList [DInteger 2],DInteger (-80)]),("nFtYxRYCdF",DMap [("dLdRUnwMN",DInteger (-82)),("ckZdxA",DInteger (-43)),("A",DMap [("T",DInteger (-82)),("molJcfwxDW",DString "aQ4pmKj  W"),("ZCKvEVj",DString "i"),("piljSJzLu",DMap [("V",DMap [("OotjoHOJ",DMap [("NjMsiSxni",DMap [("BdjqTho",DList [DMap [("qtHlLVndMf",DInteger 34),("QnhcdYz",DInteger (-54)),("gJPyYnkFS",DString "28Rv3X6U ")],DList [DList [DString " Ju 6A",DInteger 19],DMap [("zPpzHCxeFH",DList [DList [DString " Vxjm q"],DInteger (-59),DMap [("UKSxsRD",DString "  an k 5TfQ0U6 "),("dFhE",DMap [("XoLZkoAa",DMap []),("ouWKwtSSM",DInteger (-89)),("VSmTf",DMap [("QIQnbT",DString "DnX 2D0Qkj1Di"),("jPgrQqY",DString "8wxdk9 N "),("vbrRoVbrLX",DString "51Dij")]),("zXkmqwYU",DMap [])])],DInteger (-91)]),("HtzXpLUXs",DString "kM   50U"),("ahNje",DString "Gm6")],DInteger (-29)],DString "G F 0tt SV Yn ",DList [DInteger (-57)]]),("RjSlJ",DInteger (-78)),("o",DInteger 23),("XCIiORQypQ",DString " O1m")])]),("FR",DMap [("JO",DMap [("AqLJqRNGH",DInteger 98),("kSssj",DList [])]),("YpWHbPame",DInteger (-10))]),("iERHmDjx",DInteger (-71))]),("jm",DInteger (-90))])])]),("cPA",DList []),("EwwooDrRcS",DMap [("A",DInteger 29),("LZeyQxcmzZ",DList [DInteger 87,DInteger 14,DList []]),("IUA",DList [DMap [("ipaUUw",DMap [("bSrFbSK",DMap []),("ZuMzNTN",DString "MR "),("emuyQdIO",DInteger 62),("lejxGvILcd",DMap [("biaBZkC",DMap [("OhTd",DMap [("e",DInteger 81),("dUQLObZiAn",DString "5 w"),("OmN",DInteger 10),("ikUrZZjTLY",DString " VV72R  81Ly d5w")]),("guy",DList [DString "V",DInteger (-73)]),("HpcjtG",DList [DInteger (-14),DInteger (-84),DString "6  L",DInteger (-92)]),("jqwhjsFm",DInteger 56)]),("Apy",DString "2 0R0BZ8 "),("vNu",DString "O 4cGL  83Q"),("KJLIz",DMap [("POcJFBdNW",DList [DInteger 78]),("uA",DList [DString "b Am8DDp 6d89"])])])])]]),("JR",DInteger 94)])])]),("YiD",DString "TP p2Nyqe"),("eUkuse",DMap [("kke",DInteger (-29)),("dyqNI",DString "uah"),("A",DList [])])],DString "    ",DList []],DMap [],DInteger (-57)])],DInteger (-82),DInteger (-96)],DString "6AFGhowPoMhscFLJ",DString "3 ",DList [DMap [],DMap [("QCQA",DInteger 50),("nGXFBcFrF",DMap [("RRdMQP",DMap [("ZSwZpqNs",DInteger (-66))]),("kmMFv",DString "cvqhy   b5"),("dH",DList []),("b",DMap [("CnyGzgxT",DList [DString "R UvF7",DString "Ehg8se3 W gs"]),("dEj",DInteger 51)])])],DInteger 40]],DMap [("mdbJif",DMap [("qYgU",DString "2024XXVW2I65 6 g"),("MKFBjhvN",DString "gE9G OunhpEB"),("RUxISovAE",DMap [("a",DInteger 90),("Uzwms",DString "vF0o "),("NAjKVW",DMap [("rUZDkxbwq",DList [DInteger 82,DMap [("fDOFUK",DList [DInteger (-30),DList [DInteger (-19),DInteger 60,DList [DMap [],DInteger (-16)]],DString " Cuvc"]),("kWroYLadQ",DInteger 50)],DInteger (-84),DInteger 36])]),("drRnYOEMJ",DString "")]),("Okmoicz",DInteger (-24))]),("QKu",DList [DInteger 78,DList [],DList [],DMap [("BTip",DList [DInteger 94,DString "3 0f 6RHh 02K  "]),("fdwWyoUh",DInteger (-91)),("Enq",DString "s6 Q n O5 Dzz")]])]]),("xbskJUGU",DInteger (-16))]]),("qckcoFwn",DString "E "),("pmQcO",DString "X 5")]),("AIMzrrmYq",DInteger (-96)),("sXn",DMap [("trEiGBL",DMap [("YmjeoYOxPQ",DList [])]),("xruQzQNB",DMap [("Ue",DInteger (-41)),("uNAdijM",DList [DMap [("zIrgNAgI",DString " mh  gK J7 "),("Ha",DList [DMap [("Qb",DList []),("eM",DList [DInteger (-77),DInteger 63,DMap [("f",DInteger 22),("yG",DList [DInteger 24,DList [],DString " Tj0JbE ",DString "0 Yn6ip 8B"]),("qyhcVK",DString "   8ePx ")]])],DList []]),("TcRoy",DInteger 40),("PsAmzdC",DMap [("cuCNN",DMap []),("xWRjVm",DList [DMap []])])],DList []])]),("MYWb",DString " T UqV6y9")]),("dwhqiwo",DInteger 57)]

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