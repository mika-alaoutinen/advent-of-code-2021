module Day4

type Cell = { Marked: bool; Value: int }
type Board = Cell [,]
type BingoCard = { Id: int; Board: Board }

module BingoBoard =
    let private createCell x = { Marked = false; Value = x }

    let create id numbers =
        let board = numbers |> Array2D.map createCell
        { Id = id; Board = board }

module Action =
    let private markCell { Marked = marked; Value = value } n =
        if n = value then
            { Marked = not marked; Value = value }
        else
            { Marked = marked; Value = value }

    let mark { Id = id; Board = board } n =
        let newBoard = Array2D.map (fun cell -> markCell cell n) board
        { Id = id; Board = newBoard }

module Logic =
    let private isRowMarked row =
        Array.forall (fun cell -> cell.Marked) row

    let isBingo { Board = board } =
        let bingoRows =
            [ 0 .. Array2D.length1 board - 1 ]
            |> List.map (fun i -> isRowMarked board[i, *])
            |> List.exists id

        let bingoColumns =
            [ 0 .. Array2D.length2 board - 1 ]
            |> List.map (fun i -> isRowMarked board[*, i])
            |> List.exists id

        bingoRows || bingoColumns

module Bingo =
    let private playRound boards n =
        List.map (fun board -> Action.mark board n) boards

    let private checkBingo boards n =
        List.tryFind Logic.isBingo boards
        |> Option.map (fun board -> (board, n))

    let rec play initialBoards numbers =
        match numbers with
        | [] -> None
        | x :: xs ->
            let boards = playRound initialBoards x
            Option.orElse (play boards xs) (checkBingo boards x)

module Score =
    let private boardScore { Board = board } =
        board
        |> Seq.cast<Cell>
        |> Seq.filter (fun { Marked = marked } -> not marked)
        |> Seq.map (fun { Value = v } -> v)
        |> Seq.sum

    let calculate winnerOpt =
        winnerOpt
        |> Option.map (fun (board, n) -> (boardScore board, n))
        |> Option.map (fun (score, n) -> score * n)
        |> Option.defaultValue 0

    let winnerMsg winnerOpt =
        winnerOpt
        |> Option.map (fun (card, _) -> card.Id)
        |> Option.map (fun cardId -> "Winning bingo card's id: " + string cardId)
        |> Option.defaultValue "No winner"

let solve () =
    let board1 =
        [ [ 22; 13; 17; 11; 0 ]
          [ 8; 2; 23; 4; 24 ]
          [ 21; 9; 14; 16; 7 ]
          [ 6; 10; 3; 18; 5 ]
          [ 1; 12; 20; 15; 19 ] ]

    let board2 =
        [ [ 3; 15; 0; 2; 22 ]
          [ 9; 18; 13; 17; 5 ]
          [ 19; 8; 7; 25; 23 ]
          [ 20; 11; 10; 24; 4 ]
          [ 14; 21; 16; 12; 6 ] ]

    let board3 =
        [ [ 14; 21; 17; 24; 4 ]
          [ 10; 16; 15; 9; 19 ]
          [ 18; 8; 23; 26; 20 ]
          [ 22; 11; 13; 6; 5 ]
          [ 2; 0; 12; 3; 7 ] ]

    let boards =
        [ board1; board2; board3 ]
        |> List.map array2D
        |> List.mapi (fun i board -> BingoBoard.create (i + 1) board)

    let bingoNumbers =
        [ 7
          4
          9
          5
          11
          17
          23
          2
          0
          14
          21
          24
          10
          16
          13
          6
          15
          25
          12
          22
          18
          20
          8
          19
          3
          26
          1 ]

    let winner = Bingo.play boards bingoNumbers
    let winnerMsg = Score.winnerMsg winner
    let finalScore = Score.calculate winner

    printfn "%s" winnerMsg
    printfn "Final score %i" finalScore
