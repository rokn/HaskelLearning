import qualified Data.List as L

data Animal = Giraffe
            | Elephant
            | Tiger
            | Flea

type Zoo = [Animal]

localZoo :: Zoo
localZoo = [ Elephant
            , Tiger
            , Tiger
            , Giraffe
            , Elephant
            ]

adviceOnEscape :: Animal -> String
adviceOnEscape animal =
    case animal of
        Giraffe  -> "Look up"
        Elephant  -> "Ear to the ground"
        Tiger  -> "Check the morgues"
        Flea  -> "Don't worry"

adviceZooOnEscape :: Zoo -> [String]
adviceZooOnEscape = map adviceOnEscape

main = putStrLn stringToPrint
    where
        stringToPrint = L.intercalate ", " advices
        advices = adviceZooOnEscape localZoo
