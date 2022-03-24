module Day14

module ListUtil =
    let flattenTuples tuples =
        tuples
        |> Seq.collect (fun (left, right) -> [ left; right ])
        |> Seq.toList

// String that is exactly 2 chars long
module PolymerString =
    type T = PolymerString of string

    let create str =
        if str <> null && String.length str = 2 then
            Some(PolymerString str)
        else
            None

    let fromChars (a: char) (b: char) = PolymerString(string a + string b)

    let apply f (PolymerString str) = f str
    let value str = apply id str
    let fst str = str |> value |> (fun s -> s[0])
    let snd str = str |> value |> (fun s -> s[1])

type PolymerCount = Map<PolymerString.T, int>
type PolymerPair = Map<PolymerString.T, (PolymerString.T * PolymerString.T)>

module PolymerStringParser =
    // "NNCB" -> ["NN"; "NC"; "CB"]
    let private splitToCharPairs str =
        seq { for i in 0 .. String.length str - 2 -> (string str[i]) + (string str[i + 1]) }

    let toPolymerStrings str =
        str
        |> splitToCharPairs
        |> Seq.map PolymerString.create
        |> Seq.choose id
        |> Seq.toList

module PolymerPair =
    type private InsertionRule = (PolymerString.T * char)

    type private PolymerMapping =
        { Polymer: PolymerString.T
          Left: PolymerString.T
          Right: PolymerString.T }

    let private polymerBuilder ((polymer, char): InsertionRule) =
        { Polymer = polymer
          Left = PolymerString.fromChars (PolymerString.fst polymer) char
          Right = PolymerString.fromChars char (PolymerString.snd polymer) }

    let createPolymerPairs rules =
        rules
        |> List.map polymerBuilder
        |> Seq.map (fun { Polymer = p; Left = l; Right = r } -> p, (l, r))
        |> Map.ofSeq

module Input =
    type private Rule = (string * char)

    type T =
        { Polymers: PolymerCount
          Pairs: PolymerPair }

    let private createInsertionRule ((str, char): Rule) =
        str
        |> PolymerString.create
        |> Option.map (fun s -> (s, char))

    let private createInsertionRules pairs =
        pairs
        |> List.map createInsertionRule
        |> List.choose id

    let parse polymerTemplate rules =
        let polymers =
            polymerTemplate
            |> PolymerStringParser.toPolymerStrings
            |> Seq.map (fun polymer -> (polymer, 1))
            |> Map.ofSeq

        let polymerPairs =
            rules
            |> createInsertionRules
            |> PolymerPair.createPolymerPairs

        { Polymers = polymers
          Pairs = polymerPairs }

module Simulation =
    let private increment counts polymer =
        let value =
            counts
            |> Map.tryFind polymer
            |> Option.map ((+) 1)
            |> Option.defaultValue 1

        Map.add polymer value counts

    let private runIteration polymerCounts polymerPairs =
        polymerCounts
        |> Map.keys
        |> Seq.map (fun polymer -> Map.find polymer polymerPairs)
        |> ListUtil.flattenTuples
        |> List.fold increment Map.empty

    let run (polymers: PolymerCount) (polymerPairs: PolymerPair) (iterations: int) =
        seq { 1..iterations }
        |> Seq.fold (fun counts _ -> runIteration counts polymerPairs) polymers

module ElementCounts =
    type private CharCount = (char * int)
    type Result = { Min: CharCount; Max: CharCount }

    let private countPolymerChars polymerCounts polymer : (CharCount * CharCount) =
        let count = Map.find polymer polymerCounts
        let firstCharCount = (PolymerString.fst polymer, count)
        let secondCharCount = (PolymerString.snd polymer, count)
        (firstCharCount, secondCharCount)

    let private reduceDuplicateChars (charCounts: list<CharCount>) =
        charCounts
        |> List.groupBy (fun (char, _) -> char)
        |> List.map (fun (char, charCounts) -> (char, List.sumBy (fun (_, count) -> count) charCounts))

    // +1 to N and B counts, because they are first and last letters in initial polymer
    let private plusOne charCounts =
        charCounts
        |> List.map (fun (char, count) ->
            if char = 'N' || char = 'B' then
                (char, count + 1)
            else
                (char, count))

    let private charCounts (polymerCounts: PolymerCount) =
        polymerCounts
        |> Map.keys
        |> Seq.map (countPolymerChars polymerCounts)
        |> ListUtil.flattenTuples
        |> reduceDuplicateChars
        |> plusOne

    let private leastCommon polymerCounts =
        polymerCounts
        |> charCounts
        |> List.minBy (fun (_, v) -> v)

    let private mostCommon polymerCounts =
        polymerCounts
        |> charCounts
        |> List.maxBy (fun (_, v) -> v)

    let findLeastAndMostCommon (polymers: PolymerCount) =
        { Min = leastCommon polymers
          Max = mostCommon polymers }

// Not even close to being correct
let solve () =
    let polymerTemplate = "NNCB"

    let rules =
        [ ("CH", 'B')
          ("HH", 'N')
          ("CB", 'H')
          ("NH", 'C')
          ("HB", 'C')
          ("HC", 'B')
          ("HN", 'C')
          ("NN", 'C')
          ("BH", 'H')
          ("NC", 'B')
          ("NB", 'B')
          ("BN", 'B')
          ("BB", 'N')
          ("BC", 'B')
          ("CC", 'N')
          ("CN", 'C') ]

    let iterations = 10

    let input = Input.parse polymerTemplate rules
    let polymerCounts = Simulation.run input.Polymers input.Pairs iterations
    ElementCounts.findLeastAndMostCommon polymerCounts
