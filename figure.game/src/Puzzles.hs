module Puzzles
  ( puzzles )
where

puzzles :: [(Int, String)]
puzzles =
  [ ( 1
    , unlines
      [ "G G G G G"
      , "R R R R R"
      , "W W W W W"
      , "Y Y Y Y Y"
      , "G G G G G"
      ]
    )
  , ( 2
    , unlines
      [ "G G G G G"
      , "R R R R R"
      , "W W W W W"
      , "Y Y Y Y Y"
      , "G R W Y G"
      ]
    )
  , ( 75
    , unlines
      [ "R R R W W"
      , "G W W G R"
      , "W Y W Y G"
      , "R Y Y R R"
      , "R Y G G Y"
      ]
    )
  , ( 76
    , unlines
      [ "R G W W R"
      , "W W W G Y"
      , "R R R Y W"
      , "Y R R Y R"
      , "G R Y G W"
      ]
    )
  , ( 77
    , unlines
      [ "G Y G G W"
      , "Y W W G Y"
      , "Y R W Y W"
      , "G R Y Y W"
      , "R Y W R Y"
      ]
    )
  , ( 78
    , unlines
      [ "G Y W G Y"
      , "Y W W G Y"
      , "W G G W W"
      , "W G Y G R"
      , "W W G R G"
      ]
    )
  , ( 79
    , unlines
      [ "Y W G G R"
      , "Y R R G G"
      , "G Y G W W"
      , "W G G W G"
      , "W G W R W"
      ]
    )
  ]

