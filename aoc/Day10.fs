module Day10

module Parenthesis =
    let isOpen char =
        [ '('; '['; '{'; '<' ] |> List.contains char

    let closing c =
        match c with
        | '(' -> ')'
        | '[' -> ']'
        | '{' -> '}'
        | '<' -> '>'
        | _ -> failwith "Invalid char given"

module Points =
    let private score invalidChar =
        match invalidChar with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> failwith "Invalid char given"

    let calculate invalidChars =
        invalidChars |> List.map score |> List.sum

module Validator =
    let rec private findFirstCorruptedChar stack line =
        match stack, line with
        | _, [] -> None
        | accumulator, x :: xs ->
            if Parenthesis.isOpen x then
                findFirstCorruptedChar (Parenthesis.closing x :: accumulator) xs
            elif x = List.head accumulator then
                findFirstCorruptedChar (List.tail accumulator) xs
            else
                Some x

    let findCorruptedChars lines =
        lines
        |> List.map Seq.toList
        |> List.map (fun line -> findFirstCorruptedChar [] line)
        |> List.choose id

let solve () =
    let input =
        [ "[({(<(())[]>[[{[]{<()<>>"
          "[(()[<>])]({[<{<<[]>>("
          "{([(<{}[<>[]}>{[]{[(<()>"
          "(((({<>}<{<{<>}{[]{[]{}"
          "[[<[([]))<([[{}[[()]]]"
          "[{[{({}]{}}([{[{{{}}([]"
          "{<[[]]>}<{[{[{[]{()[[[]"
          "[<(<(<(<{}))><([]([]()"
          "<{([([[(<>()){}]>(<<{{"
          "<{([{{}}[<[[[<>{}]]]>[]]" ]

    let points =
        input
        |> Validator.findCorruptedChars
        |> Points.calculate

    printfn "Total syntax error score: %i" points
