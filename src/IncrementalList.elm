module IncrementalList exposing (range)

{-|
# Range generator

@docs range
-}


{-| Create range of values using custom increase/decrease logic,
relative value and number of succeeding/preceding values to be generated.

Use positive number to generate succeeding values, and
negative number to generate values preceding the provided value:

    -- Integers
    prevInt = \n -> n - 1
    nextInt = (+) 1
    intRange = range prevInt nextInt

    intRange 1 5 == [1,2,3,4,5]
    intRange 5 -5 == [1,2,3,4,5]

    -- Characters
    prevChar = Char.toCode >> (+) -1 >> Char.fromCode
    nextChar = Char.toCode >> (+) 1 >> Char.fromCode
    charRange = range prevChar nextChar

    charRange 'A' 3 == ['A', 'B', 'C']
    charRange 'C' -3 == ['A', 'B', 'C']

-}
range : (a -> a) -> (a -> a) -> a -> Int -> List a
range decreaser increaser startingElement length =
    case compare length 0 of
        GT ->
            List.repeat (length - 1) startingElement
                |> List.foldl
                    (\_ acc ->
                        increaser (Maybe.withDefault startingElement (List.head acc))
                            :: acc
                    )
                    [ startingElement ]
                |> List.reverse

        LT ->
            List.repeat (-length - 1) startingElement
                |> List.foldl
                    (\_ acc ->
                        decreaser (Maybe.withDefault startingElement (List.head acc))
                            :: acc
                    )
                    [ startingElement ]

        EQ ->
            []
