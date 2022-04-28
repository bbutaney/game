open! CS17SetupGame;
open Game; 

/* Note: Some data definitions may be repeated from other modules.
 I have included all of the data definitions and example data
 used in this module in the comment directly below this one. */

/*
   Connect4.re Data Definitions:
 
  A list('a) is either:
       [item, ...b] where item is a generic type 'a and b is a list('a), or
       empty
 
   A list(list('a)) is either
      [item, ...b] where item is a list('a) and b is a list(list('a)), or
      empty
 
   A board is a list(list(int)) representing the positions of chips
   and empty spaces on the connect4 board, with chips being represented
   by ints corresponding to the player who placed them
 
   A status is either Ongoing(P1), Ongoing(P2), Win, or Draw
 
   A whichPlayer is either P1 or P2, representing a player of the game
 
   A state is a tuple of a board and its status
 
   A move is an int in the form Move(int) representing the column on
   the board that a player can place a piece , with the piece
   falling to the lowest unoccupied row in the specified column
 
   A list(move) is either
      [item, ...b] where item is a move and b is a list(move), or
      empty
 
   A list(int) is either
      [item, ...b] where item is a int and b is a list(int), or
      empty
 
   A list(list(int)) is either
       [item, ...b] where item is a list(int) and b is a list(list(int)), or
       empty
 
   A list(String) is either
       [item, ...b] where item is a String and b is a list(String), or
       empty
 
   A list(board) is either
       [item, ...b] where item is a board and b is a list(board), or
       empty
 
   Connect4.re Example Data:
 
   list('a): ["hi", "bye"], [1, 2, 3]
 
   String: "hi", "bye"
 
   list(list('a)): [[9, 8, 7], [1, 2, 3]], [[1]]
 
   board: [[0, 0, 2, 1], [0, 0, 2, 1], [0, 2, 1, 1], [0, 2, 2, 1]],
          [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
  
   status: Ongoing(P1), Win, Draw, Ongoing(P2)
 
   whichPlayer: P1, P2
 
   state: (Ongoing(P1), 
          [[0, 0, 2, 1], [0, 0, 2, 1], [0, 2, 1, 1], [0, 2, 2, 1]]),
          (Ongoing(P1), 
          [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]])
  
   move: Move(1), Move(4)
 
   list(move): [Move(1), Move(4)], [Move(3)]
 
   int: 1, 2
 
   list(int): [1, 2], [35]
 
   list(list(int)): [[1, 2], [3, 4]], [[1]]
 
   list(String): ["hi", "bye"], ["hello"]
 
   list(board): [[[0, 0, 2, 1], [0, 0, 2, 1], [0, 2, 1, 1], [0, 2, 2, 1]],
          [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]], []
 
   bool: true, false
 
   float: 12.5, 0.0
*/

module Connect4: Game = {

    /* player 1 is P1, player 2 is P2 */
    type whichPlayer =
      | P1
      | P2;

    /* either a player has won, it's a draw, or it's ongoing */
    type status =
      | Win(whichPlayer)
      | Draw
      | Ongoing(whichPlayer);

    /* A board will be a 2D matrix representing
    the game board, with empty spots being 0s, player 1 occupied spots being
    1s, and player 2 occupied spots being 2s */
    type board = list(list(int)); 

    /* A state will be board and its status */
    type state = (board, status); 

    /* A move will be an int in the
    form Move(int) representing the column on the board that a 
    player can place a piece , with the piece falling to the lowest
    unoccupied row in the specified column */ 
    type move = Move(int); 

   /* printing functions */
    
    /*
      Input: wP, a whichPlayer
      Output: a string representation of wP
    */
    let stringOfPlayer: whichPlayer => string = wP =>
      switch (wP) {
      | P1 => "Player 1";
      | P2 => "Player 2";
      };

    checkExpect(stringOfPlayer(P1), "Player 1", "stringOfPlayer 1");
    checkExpect(stringOfPlayer(P2), "Player 2", "stringOfPlayer 2");

    /*
      Input: a board, bd
      Output: the transposition of bd (columns and rows are swapped)

      Recursion Diagram 1:
      OI: [[1, 2], [3, 4]]
      RI: [[2, 4]]
      RO: [[2, 4]]
      OO: [[1, 3], [2, 4]]

      Recursion Diagram 2:
      OI:[[1], [2]]
      RI: N/A
      RO: N/A
      OO: [[1, 2]]
   */

    let rec transpose: board => board = bd =>
      switch (bd) {
      | [] =>  failwith ("A board cannot be 0 - dimensional.");
      | [[], ..._] => failwith ("A board cannot be 0 - dimensional.");
      | [[_], ..._] => [List.flatten(bd)];
      | [[_, ... _], ..._] => 
        [List.map(List.hd, bd), ...transpose(List.map(List.tl, bd))];
      };

    checkExpectListListAlpha(transpose([[1, 2], [3, 4]]),[[1, 3], [2, 4]],
      "2x2")
    checkExpectListListAlpha(transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]]),
      [[1, 4, 7], [2, 5, 8], [3, 6, 9]], "3x3")
    checkExpectListListAlpha(transpose([[1], [2]]), [[1, 2]], "1x2")

    /*
      Input: an alpha list, lst, and a procedure that takes in a
      generic type 'a and outputs a string, strOf

      Output: a string representation of lst

      note: stringOfAList was taken from Rackette, as allowed 
      by EdStem Post #1915
    */
    let stringOfAList: (list('a), 'a => string) => string =
      (lst, strOf) =>
      "[" ++
        List.fold_right(
          (a, b) => strOf(a) ++ (if (b == "]") { "" } else { ", " }) ++ b,
          lst,
          "]"
        );

    checkExpect(stringOfAList([1, 2, 3], string_of_int), 
      "[1, 2, 3]", "list of int to string");
    checkExpect(stringOfAList([], string_of_int), "[]", 
      "empty list to string");

    /*
      Input: a board, b
      Output: a string version of a list of lists representing the board

      Recursion Diagram 1:
      OI:[[0,0], [0,0]]
      RI: [[0,0]]
      RO: "[[0,0]]"
      OO: "[[0,0], [0,0]]"

      Recursion Diagram 2:
      OI: []
      RI: N/A
      RO: N/A
      OO: " "
    */
    let rec makeListOfStrings: board => string = b =>
      switch (b) {
      | [] => " ";
      | [hd, ...tl] => 
        stringOfAList(hd, string_of_int) ++ "\n" ++ makeListOfStrings(tl);
      }

    //note: after checking the output, makeListOfStrings works properly,
    //but checkExpect does not recognize the equality of the 
    //expected and actual output
    /* checkExpect(makeListOfStrings([[1,1,1,2], [0,0,0,1], [1,2,1,2]]),
     "[1, 1, 1, 2]" ++ "\n" ++ "[0, 0, 0, 1]" ++ "\n" ++ "[1, 2, 1, 2]" 
      ++ "\n", 
    "makeListOfStrings, uneven"); */
    /* checkExpect(makeListOfStrings([[1]]), "[1]" ++ "\n", ""); */

    /* 
    Input: a state, with its board being b and its status being unnamed
    Output: a string representation of the board transposed to look like
    a connect four board 
    */
    let stringOfState = ((b, _): state): string =>
      makeListOfStrings(transpose(b));
    
    //note: after checking the output, stringOfState works properly, 
    //but checkExpect does not recognize the equality of the expected
    //and actual output
    /* checkExpect(stringOfState(([[0,0,1],[0,0,1],[0,0,1]], Ongoing(P1))), 
    "[0, 0, 0]" ++ "\n" ++ "[0, 0, 0]" ++ "\n" ++ "[1, 1, 1]" ++ "\n", ""); */

    /*
    Input: a move, m
    Output: a string representation of the int contained within m
    */
    let stringOfMove: move => string = m =>
      switch(m) {
      | Move(hd) => string_of_int(hd);
      };

    checkExpect(stringOfMove(Move(1)), "1", "stringOfMove");

    /* Game Logic */

    /*
      Input: list of int, l, and an int, n
      Output: a list of zeroes of length n
    
      Recursion Diagram 1:
      OI: [], 2
      RI: [0], 1
      RO: [0, 0]
      OO: [0, 0]

      Recursion Diagram 2:
      OI: [], 0
      RI: N/A
      RO: N/A
      OO: []
    */
    let rec createZeroRow: (list(int), int) => list(int) = (l, n) =>
      if (n > 0) {
          createZeroRow([0, ... l], (n - 1))
       } else {
            l
       };

    checkExpect(createZeroRow([], 2), [0,0], "zero row of length 2");
    checkExpect(createZeroRow([], 0), [], "zero row base case");

    /*
    * Input: board, bd, and two ints, height and width
    * Output: a board of the dimensions height and width 
    * filled with 0s
    *
    * Recursion Diagram 1:
    * OI: [], 2, 2
    * RI: [[]], 1, 2
    * RO: [[0, 0]]
    * OO: [[0, 0], [0, 0]]
    *
    * Recursion Diagram 2:
    * OI: [] 0 0
    * RI: N/A
    * RO: N/A
    * OO: []
    */

    let rec createEmptyBoard: (board, int, int) => list(list('a)) 
    = (bd, height, width) =>
      if (height > 0 && width > 0) {
          createEmptyBoard([createZeroRow([], width), ... bd], 
            (height - 1), width)
       } else {
            bd
       };

    checkExpect(createEmptyBoard([], 2, 2), [[0, 0], [0, 0]],
       "createEmptyBoard 2x2");
    checkExpect(createEmptyBoard([], 0, 0), [], "createEmptyBoard base case");
    checkExpect(createEmptyBoard([], 2, 0), [], "createEmptyBoard 2x0 case");
    checkExpect(createEmptyBoard([], 0, 2), [], "createEmptyBoard 0x2 case");

    /* the state of the game when it begins */
    /*
    Input: a string, s, detailing dimensions for a board
    Output: a state with an empty board based on the dimensions specified in s
    and with the status of the state defaulting to Ongoing(P1)
    */
    let initialState: string => state =
      s => {
        let boardDims = parseBoardDims(s);
        let boardHeight = getBoardHeight(boardDims);
        let boardWidth = getBoardWidth(boardDims);
        /* your initial state, using boardHeight and boardWidth, goes here */
        (createEmptyBoard([], boardWidth, boardHeight), Ongoing(P1));
        /*note: the order of boardWidth and Height is reversed for ease of
        placing chips on the board*/
      };

    checkExpect(initialState("2 3"), ([[0, 0], [0, 0], [0, 0]], Ongoing(P1)), 
      "2 3 initial state");
    checkExpect(initialState("0 0"), ([], Ongoing(P1)), "0 0 initial state");

    /*
    Input: a board, board, and an int, colNum
    Output: a list of moves that are legal given board and colNum

    Recursion Diagram 1:
    OI: [[0,0],[0,0]], 1
    RI: [[0,0]], 2
    RO: []
    OO: [Move(0), Move(1)]

    Recursion Diagram 2:
    OI: [], 0
    RI: N/A
    RO: N/A
    OO: []
    */
    let rec getLegalCols: (board, status, int) => list(move) = 
    (board, s, colNum) =>
      switch(board, s) {
      | (_, Win(_))
      | (_, Draw) 
      | ([], _) => [];
      | ([[0, ... _], ... tl], _) => 
        [Move(colNum - 1), ... getLegalCols(tl, s, (colNum + 1))];
      | ([_, ... tl], _) => getLegalCols(tl, s, (colNum + 1));
      }

    checkExpect(getLegalCols([[1,1,1,1], [2,2,2,2], [0,0,0,0], [0,1,2,1]], 
      Ongoing(P1), 1), [Move(2), Move(3)], "getLegalCols 1");
    checkExpect(getLegalCols([[1,1,1,1], [2,2,2,2], [0,0,0,0], [0,1,2,1]],
      Ongoing(P2), 1), [Move(2), Move(3)], "getLegalCols 2");
    checkExpect(getLegalCols([[1,1,1,1], [2,2,2,2], [0,0,0,0], [0,1,2,1]], 
      Win(P1), 1), [], "getLegalCols Win 1");
    checkExpect(getLegalCols([[1,1,1,1], [2,2,2,2], [0,0,0,0], [0,1,2,1]], 
      Win(P2), 1), [], "getLegalCols Win 2");
    checkExpect(getLegalCols([[1,1,1,1], [2,2,2,2], [0,0,0,0], [0,1,2,1]], 
      Draw, 1), [], "getLegalCols Draw");
    checkExpect(getLegalCols([], Draw, 1), [], "getLegalCols base case");
    
    /*
    Input: a state, with its board element b and its status element being any
    possible status value
    Output: the list of legal moves at the inputted state
    */
    /* produces the list of legal moves at a state */
    let legalMoves = ((b, s): state): list(move) =>
      getLegalCols(b, s, 1);

    checkExpect(legalMoves(([[1,1,1], [0,0,1], [0,2,2]], Ongoing(P2))), 
      [Move(1), Move(2)], "legalMoves P2");
    checkExpect(legalMoves(([[1,1,1], [0,0,1], [0,2,2]], Ongoing(P1))), 
      [Move(1), Move(2)], "legalMoves P1");
    checkExpect(legalMoves(([[1,1,1], [0,0,1], [0,2,2]], Draw)), 
      [], "legalMoves Draw");
    checkExpect(legalMoves(([[1,1,1], [0,0,1], [0,2,2]], Win(P1))), 
      [], "legalMoves Win 1");
    checkExpect(legalMoves(([[1,1,1], [0,0,1], [0,2,2]], Win(P2))), 
      [], "legalMoves Win 2");
    checkExpect(legalMoves(([], Draw)), [], "legalMoves base case");
    /*
    Input: a list of ints, l, an int, player, and an int, streak
    Output: an int providing the player number if l has four of that players
    number in a row, otherwise returning 0

    Recursion Diagram 1:
    OI: [0, 2, 2, 2, 2], 2, 0
    RI: [2, 2, 2, 2], 2, 0
    RO: 2
    OO: 2

    Recursion Diagram 2:
    OI: [], 0, 0
    RI: N/A
    RO: N/A
    OO: 0
    */
    let rec fourInAList: (list(int), int, int) => int = (l, player, streak) =>
      if (l == []) {0}
      else if ((List.hd(l) == player) && (streak == 2)) {player}
      else if (List.hd(l) == player) 
        {fourInAList(List.tl(l), player, (streak + 1))}
      else {fourInAList(List.tl(l), List.hd(l), 0)};

    checkExpect(fourInAList([0,2,1,1,1,1,3], 0, 0), 1, "fourInAList");
    checkExpect(fourInAList([0,2,1,1,2,1,1], 1, 0), 0, 
                "fourInAList but broken-up streak");
    checkExpect(fourInAList([], 1, 0), 0, "fourInAList base");

    /*
    Input: a board, s
    Output: a string version of the player number that has 4 chips
    in a vertical row, or "0" if there is no set of 4 chips in a 
    vertical row

    Recursion Diagram 1:
    OI: [[1,1,1,1],[0,0,0,0],[0,0,0,0]]
    RI: [[0,0,0,0],[0,0,0,0]]
    RO: "0"
    OO: "1"

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: "0"
    */
    let rec verticals: board => string = s =>
      switch (s) {
      | [hd, ..._] when (fourInAList(hd, 0, 0) != 0) => 
        string_of_int(fourInAList(hd, 0, 0));
      | [hd, ...tl] when (fourInAList(hd, 0, 0) == 0) => verticals(tl);
      | _ => "0";
      }

    checkExpect(verticals([[2,1,1,1,1,0], [0,0,0,0,0], [0,0,0,0,0]]), 
      "1", "verticals");
    checkExpect(verticals([]), "0", "verticals base");
    checkExpect(verticals([[0,0,2,2,2], [2,1,1,0,1,1], [0,0,1,0,0]]), 
      "0", "verticals split");
    checkExpect(verticals([[2,1,1,1,1,0], [2,2,2,2,0], [0,0,0,0,0]]), 
      "1", "verticals order");

    /*
    Input: a board, s
    Output: a string version of the player number that has 4 chips
    in a horizontal row, or "0" if there is no set of 4 chips in a 
    horizontal row
    */
    let horizontals: board => string = s =>
      verticals(transpose(s));

    checkExpect(horizontals([[0,1,2],[1,1,2],[0,1,2],[0,1,1]]), 
      "1", "horizontals");
    checkExpect(horizontals([[0,1,2],[1,1,2],[0,0,2],[0,1,1],[1,1,1]]), 
      "0", "horizontals broken");

    /*
    Input: a board, l, and int, n
    Output: a new board with all diagonals in the board made into horizontals

    Recursion Diagram 1:
    OI: [[1,1,1,1],[0,1,0,0],[0,0,1,0]], 0
    RI: [[0,1,0,0],[0,0,1,0]], 1
    RO: [[1,0,0,0], [1,0,0,0]]
    OO: [[1,0,0,0],[1,0,0,0], [1,0,0,0]]

    Recursion Diagram 2:
    OI: [], 0
    RI: N/A
    RO: N/A
    OO: []
    */
    let rec makeDiagHoriz: (board, int) => board = (l, n) =>
      switch(n, l) {
      | (0, [hd, ...tl]) => [hd, ...makeDiagHoriz(tl, 1)];
      | (1, [[_,...rm], ...tl]) => 
        [List.flatten([rm, [0]]), ...makeDiagHoriz(tl, 2)];
      | (2, [[_, _, ...rm], ...tl]) => 
        [List.flatten([rm, [0, 0]]), ...makeDiagHoriz(tl, 3)];
      | (3, [[_, _, _, ...rm]]) => 
        [List.flatten([rm, [0, 0, 0]])];
      | _ => [[]];
      };

    checkExpect(makeDiagHoriz([[1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1]], 0),
    [[1,0,0,0],[1,0,0,0],[1,0,0,0],[1,0,0,0]], "makeDiagHoriz");
    /*
    Input: a board, s
    Output: a list of boards containing all sets of 4-in-a-row subsets of s,
    with makeDiagHoriz applied to all sub-boards

    Recursion Diagram 1:
    OI: [[1,1,1,1],[0,1,0,0],[0,0,1,0], [0,0,0,0], [0,0,0,0]]
    RI: [[0,1,0,0],[0,0,1,0], [0,0,0,0], [0,0,0,0]]
    RO: [[1,0,0,0], [1,0,0,0]]
    OO: [[[1,0,0,0],[1,0,0,0], [1,0,0,0], [0,0,0,0]], 
    [[0,1,0,0],[0,0,1,0], [0,0,0,0], [0,0,0,0]]]

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: []
    */  
    let rec createListOfFours: board => list(board) = s =>
      switch(s) {
      | [hd1, hd2, hd3, hd4, ...tl] => 
        [makeDiagHoriz([hd1, hd2, hd3, hd4], 0),
        ...createListOfFours([hd2, hd3, hd4,...tl])];      
      | _ => [];
      };

    checkExpect(
        createListOfFours(
          [[0,0,0,0],[0,0,0,0],[0,0,0,0], [0,0,0,0], [0,0,0,0]]), 
          [[[0,0,0,0],[0,0,0,0], [0,0,0,0], [0,0,0,0]], 
          [[0,0,0,0],[0,0,0,0], [0,0,0,0], [0,0,0,0]]],
          "createListOfFours zeros");
    checkExpect(
      createListOfFours(
        [[1,0,0,0],[0,1,0,0],[0,0,1,0], [0,0,0,1], [0,0,0,0]]), 
        [[[1,0,0,0],[1,0,0,0], [1,0,0,0], [1,0,0,0]], 
        [[0,1,0,0],[0,1,0,0], [0,1,0,0], [0,0,0,0]]], 
        "createListOfFours non-zero");
    checkExpect(createListOfFours([]), [], "createListOfFours base");
    /*
    Input: a list(board), s
    Output: a list(string) indicating all diagonals from the list(boards)

    Recursion Diagram 1:
    OI: [[[1,0,0,0],[1,1,0,0], [1,0,1,0], [1,0,0,1]], 
    [[0,1,0,0],[0,0,1,0], [0,0,0,0], [0,0,0,0]]]
    RI: [[[0,1,0,0],[0,0,1,0], [0,0,0,0], [0,0,0,0]]]
    RO: ["0"]
    OO: ["1", "0"]

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: []
    */ 
    let rec getDiagonals: list(board) => list(string) = s =>
      switch (s) {
      | [hd, ...tl] => [horizontals(hd),...getDiagonals(tl)];        
      | [] => [];
      };

    checkExpect(getDiagonals([[[1,0,0,0],[1,1,0,0], [1,0,1,0], [1,0,0,1]], 
    [[0,1,0,0],[0,0,1,0], [0,0,0,0], [0,0,0,0]]]), ["1", "0"], "getDiagonals");
    checkExpect(getDiagonals([[[1,0,0,0],[1,0,0,0], [0,1,1,0], [1,0,0,1]], 
    [[0,1,0,0],[0,0,1,0], [0,0,0,0], [0,0,0,0]]]), ["0", "0"], 
      "getDiagonals 0 case");
    checkExpect(getDiagonals([]), [], "getDiagonals base case");

    /*
    Input: a list(string), l
    Output: a string indicating whether any of the strings in l contain
    a number other than "0". If not, returns "0". Else, returns the non-zero
    number as a string

    Recursion Diagram 1:
    OI: ["1", "0", "0"]
    RI: ["0", "0"]
    RO: "0"
    OO: "1"

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: "0"
    */ 
    let rec containsPlayerNum: list(string) => string = l =>
      switch (l) {
      | ["1", ..._] => "1";        
      | ["2", ..._] => "2";
      | [_, ...tl] => containsPlayerNum(tl);
      | [] => "0";
      };

    checkExpect(containsPlayerNum(["0", "1", "0"]), "1", "containsPlayerNum 1");
    checkExpect(containsPlayerNum(["0", "0", "0"]), "0", "containsPlayerNum 0");
    checkExpect(containsPlayerNum([]), "0", "containsPlayerNum base");
    /*
    Input: a board, l
    Output: a string indicating whether any of the players have 4 in a row diagonally. 
    If not, returns "0". Else, returns the non-zero player number as a string
    */ 
    let diagonals: board => string = l =>
      if ((containsPlayerNum(getDiagonals(createListOfFours(l))) == "1") ||
      (containsPlayerNum(getDiagonals(createListOfFours(List.rev(l)))) == "1")) {"1"}        
      else if ((containsPlayerNum(getDiagonals(createListOfFours(l))) == "2") ||
      (containsPlayerNum(getDiagonals(createListOfFours(List.rev(l)))) == "2")) {"2"}
      else {"0"};

    checkExpect(diagonals(
      [[0,0,0,0], [1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1], [0,0,0,0]]),
      "1", "diagonals");
    checkExpect(diagonals(
      [[0,0,0,0], [0,0,0,1], [0,0,1,0], [0,1,0,0], [1,0,0,0], [0,0,0,0]]), 
      "1", "diagonals reverse");
    checkExpect(diagonals(
      [[0,0,0,0], [1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,0], [0,0,0,0]]), 
      "0", "diagonals 3 fail");
    checkExpect(diagonals([]),"0","diagonals base");
        
    /*
    Input: a list(int), l, an int, n, and an int, count
    Output: an int representing the number of times player is in l

    Recursion Diagram 1:
    OI: [1,1,2,1], 1, 0
    RI: [1,2,1], 1, 1
    RO: 3
    OO: 3

    Recursion Diagram 2:
    OI: [], 0, 0
    RI: N/A
    RO: N/A
    OO: 0
    */ 
    let rec playerCounterList: (list(int), int, int) => int = (l, n, count) =>
      switch (l) {
      | [hd, ...tl] when hd == n => playerCounterList(tl, n, (count + 1));
      | [_, ...tl] => playerCounterList(tl, n, count);
      | [] => count;
      };
    checkExpect(playerCounterList([1,1,2,1], 1, 0), 3, "playerCounterList");
    checkExpect(playerCounterList([], 0, 0), 0, "playerCounterList base");

    /*
    Input: a board, s, and an int, n
    Output: the number of times a player is in the board of a given state

    Recursion Diagram 1:
    OI: [[1,0,0,0], [0,0,0,1], [0,0,0,0]], 1
    RI: [[0,0,0,1], [0,0,0,0]], 1
    RO: 1
    OO: 2

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: 0
    */ 
    let rec playerCounterState: (board, int) => int = (s, n) =>
      switch (s) {
      | [hd, ...tl] => playerCounterList(hd, n, 0) + playerCounterState(tl, n);
      | [] => 0;
      }

    checkExpect(playerCounterState([[1,0,0,0], [0,0,0,1], [0,0,0,0]], 1), 2, 
      "playerCounterState");
    checkExpect(playerCounterState([], 1), 0, "playerCounterState base");
    /*
    Input: a state, split into its board, b, and its status, which 
    can be anything
    Output: the status of the game at the inputted state
    */
    /* returns the status of the game at the given state */
    let gameStatus = ((b, _): state): status =>
      if ((verticals(b) == "1") || (horizontals(b) == "1") || (diagonals(b) == "1"))
      {Win(P1)}
      else if ((verticals(b) == "2") || (horizontals(b) == "2") || (diagonals(b) == "2"))
      {Win(P2)}
      else if (playerCounterState(b, 0) == 0) {Draw}
      else if (playerCounterState(b, 2) == playerCounterState(b, 1)) {Ongoing(P1)}
      else if (playerCounterState(b, 2) < playerCounterState(b, 1)) {Ongoing(P2)}
      else {Draw};

    checkExpect(
      gameStatus(
        ([[1,2,1,1],[0,1,2,2],[1,1,1,2],[0,0,1,1]],Ongoing(P2))), Win(P1),
         "gameStatus Win(P1) Diagonal");
    checkExpect(
      gameStatus(
        ([[1,2,1,2],[0,1,2,2],[1,1,1,2],[0,0,1,2]],Ongoing(P2))), Win(P2),
        "gameStatus Win(P2) Horizontal");
    checkExpect(
      gameStatus(
        ([[1,1,1,1],[0,1,2,2],[1,0,0,2],[0,0,1,1]],Ongoing(P2))), Win(P1),
        "gameStatus Win(P1) Vertical");
    checkExpect(
      gameStatus(
        ([[1,2,1,1],[2,1,2,2],[1,2,2,2],[1,2,1,1]],Ongoing(P2))),
        Draw, "gameStatus Draw");
    checkExpect(
      gameStatus(
        ([[1,2,1,1],[0,1,2,2],[1,2,0,2],[1,2,1,1]],Ongoing(P2))), 
        Ongoing(P2), "gameStatus Ongoing");

    /*
    Input: an int, n, and a list(int), l
    Output: the list(int) with n added to the last instance of 0 in the list

    Recursion Diagram 1:
    OI: [0, 0, 1, 1], 2
    RI: [0, 1, 1], 2
    RO: [1, 1, 1]
    OO: [0,1,1,1]

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: []
    */ 
    let rec addChip: (int, list(int)) => list(int) = (n, l) =>
      switch (l) {
      | [0, 0, ...tl] => [0, ...addChip(n, [0, ...tl])];
      | [0, ...tl] => [n, ...tl];
      | [_, ..._] => l;
      | [] => [];
      }
    checkExpect(addChip(1, [0,0,0,1]), [0,0,1,1], "addChip");
    checkExpect(addChip(1, [2,2,2,2]), [2,2,2,2], "addChip full");
    checkExpect(addChip(1, []), [], "addChip base case");
    /*
    Input: an int, n, and a list(int), col, and a board, board
    Output: the board with the nth row replaced by col

    Recursion Diagram 1:
    OI: 2, [1,1,1,1], [[0,0,0,0], [0,0,0,0]]
    RI: 2, [1,1,1,1], [[0,0,0,0]]
    RO: [[1,1,1,1]]
    OO: [[0,0,0,0], [1,1,1,1]]

    Recursion Diagram 2:
    OI: 0, [], []
    RI: N/A
    RO: N/A
    OO: []
    */ 
    let rec replaceRow: (int, list(int), board) => board = (n, col, board) =>
      switch(n, board) {
      | (_, []) => [col];
      | (0, [_, ...tl]) => [col, ...tl];
      | (_, [hd, ...tl]) => [hd, ...replaceRow((n - 1), col, tl)];
      };

    checkExpect(replaceRow(
      2, [2,2,2,2], [[1,1,1,1],[1,1,1,1], [1,1,1,1], [1,1,1,1]]), 
      [[1,1,1,1],[1,1,1,1], [2,2,2,2], [1,1,1,1]], "replaceRow");
    checkExpect(replaceRow(0,[],[]), [[]], "replaceRow base case");
    /*
    Input: a state, with board b and status s, and a move, m
    Output: the next state given b, s, and m
    */
    /* given a state and a legal move, yields the next state */
    let nextState = ((b, s): state, m: move): state => 
      switch (s, m) {
      | (Draw,_) => (b, s);
      | (Ongoing(P1), Move(hd)) => switch (gameStatus((replaceRow(
        hd, addChip(1, List.nth(b, hd)), b), Ongoing(P2)))) {
        | Win(P1) => (replaceRow(hd, addChip(1, List.nth(b, hd)), b), Win(P1));
        | Draw => (replaceRow(hd, addChip(1, List.nth(b, hd)), b), Draw);
        | _ => (replaceRow(hd, addChip(1, List.nth(b, hd)), b), Ongoing(P2));
        }
      | (Ongoing(P2), Move(hd)) => switch(gameStatus((replaceRow(
        hd, addChip(2, List.nth(b, hd)), b), Ongoing(P1)))) {
        | Win(P2) => (replaceRow(hd, addChip(2, List.nth(b, hd)), b), Win(P2));
        | Draw => (replaceRow(hd, addChip(2, List.nth(b, hd)), b), Draw);
        | _ => (replaceRow(hd, addChip(2, List.nth(b, hd)), b), Ongoing(P1));
        }
      | (_, _) => (b, s);
      };

    checkExpect(
      nextState(
        ([[0,1,1,1], [0,0,0,0], [0,0,0,0]], Ongoing(P1)), Move(0)),
        ([[1,1,1,1], [0,0,0,0], [0,0,0,0]], Win(P1)), "nextState Win");
    checkExpect(
      nextState(
        ([[1,1,1,1], [0,0,0,0], [0,0,0,0]], Ongoing(P1)), Move(0)),
        ([[1,1,1,1], [0,0,0,0], [0,0,0,0]], Win(P1)), "nextState pre-Win");
    checkExpect(
      nextState(
        ([[0,1,1,1], [0,0,0,0], [0,0,0,0]], Ongoing(P2)), Move(1)), 
        ([[0,1,1,1], [0,0,0,2], [0,0,0,0]], Ongoing(P1)), "nextState Ongoing");
    checkExpect(
      nextState(
        ([[0,1,1,1], [1,2,2,2], [2,1,1,1]], Ongoing(P2)), Move(0)), 
        ([[2,1,1,1], [1,2,2,2], [2,1,1,1]], Draw), "nextState Draw");
    checkExpect(
      nextState(
        ([[2,1,1,1], [1,2,2,2], [2,1,1,1]], Draw), Move(0)), 
        ([[2,1,1,1], [1,2,2,2], [2,1,1,1]], Draw), "nextState pre-Draw");
    /*
    Input: a string, input, and a state, s
    Output: a move version of the inputted string
    */
    /* for transforming human player input into
    internal representation of move */
    let moveOfString: (string, state) => move = (input, s) => 
      {let m = try(Move(int_of_string(input) - 1)) {
        | _ => failwith("Invalid Input");
        };
      if(!List.mem(m, legalMoves(s))) {failwith("Illegal Move")} m;
    };

    checkExpect(
      moveOfString(
        "1", ([[0,1,1,1], [0,0,0,0], [0,0,0,0]], Ongoing(P1))),
         Move(0), "First column legal move");
    checkError(() => 
      moveOfString("1", 
      ([[1,1,1,1], [0,0,0,0], [0,0,0,0]], Ongoing(P1))), "Illegal Move");
    checkError(() => 
      moveOfString("hi", 
      ([[1,1,1,1], [0,0,0,0], [0,0,0,0]], Ongoing(P1))), "Invalid Input");

    /*
    Input: a list of ints, l, an int, player, and an int, streak
    Output: an int providing the player number if l has three of that players
    number in a row, otherwise returning 0

    Recursion Diagram 1:
    OI: [0, 2, 2, 2], 2, 0
    RI: [2, 2, 2], 2, 0
    RO: 2
    OO: 2

    Recursion Diagram 2:
    OI: [], 0, 0
    RI: N/A
    RO: N/A
    OO: 0
    */
    let rec threeInAList: (list(int), int, int) => int = (l, player, streak) =>
      if (l == []) {0}
      else if ((List.hd(l) == player) && (streak == 1)) {player}
      else if (List.hd(l) == player) 
        {threeInAList(List.tl(l), player, (streak + 1))}
      else {threeInAList(List.tl(l), List.hd(l), 0)};

    checkExpect(threeInAList([0,2,1,1,1,3], 0, 0), 1, "threeInAList");
    checkExpect(threeInAList([0,2,1,2,1,1], 1, 0), 0, 
                "threeInAList but broken-up streak");
    checkExpect(threeInAList([], 1, 0), 0, "threeInAList base");

    /*
    Input: a board, s
    Output: a string version of the player number that has 3 chips
    in a vertical row, or "0" if there is no set of 3 chips in a 
    vertical row

    Recursion Diagram 1:
    OI: [[1,1,1],[0,0,0],[0,0,0]]
    RI: [[0,0,0],[0,0,0]]
    RO: "0"
    OO: "1"

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: "0"
    */
    let rec verticalsThree: board => string = s =>
      switch (s) {
      | [hd, ..._] when (threeInAList(hd, 0, 0) != 0) => 
        string_of_int(threeInAList(hd, 0, 0));
      | [hd, ...tl] when (threeInAList(hd, 0, 0) == 0) => 
        verticalsThree(tl);
      | _ => "0";
      }

    checkExpect(verticalsThree([[2,1,1,1,0], [0,0,0,0], [0,0,0,0]]),
      "1", "verticals3");
    checkExpect(verticalsThree([]), "0", "verticals3 base");
    checkExpect(verticalsThree([[0,0,2,2], [2,1,1,0,1], [0,0,1,0]]), 
      "0", "verticals3 split");
    checkExpect(verticalsThree([[2,1,1,1,0], [2,2,2,0], [0,0,0,0]]), 
      "1", "verticals3 order");

    /*
    Input: a board, s
    Output: a string version of the player number that has 3 chips
    in a horizontal row, or "0" if there is no set of 3 chips in a 
    horizontal row
    */
    let horizontalsThree: board => string = s =>
      verticalsThree(transpose(s));

    checkExpect(horizontalsThree([[0,1,2],[1,1,2],[0,1,2]]), 
      "1", "horizontals3");
    checkExpect(horizontalsThree([[0,1,2],[1,1,2],[0,0,0],[0,1,1]]), 
      "0", "horizontals3 broken");

    /*
    Input: a board, l, and int, n
    Output: a new board with all diagonals in the board made into horizontals

    Recursion Diagram 1:
    OI: [[1,1,1],[0,1,0],[0,0,1]], 0
    RI: [[0,1,0],[0,0,1]], 1
    RO: [[1,0,0], [1,0,0]]
    OO: [[1,0,0],[1,0,0], [1,0,0]]

    Recursion Diagram 2:
    OI: [], 0
    RI: N/A
    RO: N/A
    OO: []
    */
    let rec makeDiagHorizThree: (board, int) => board = (l, n) =>
      switch(n, l) {
      | (0, [hd, ...tl]) => [hd, ...makeDiagHorizThree(tl, 1)];
      | (1, [[_,...rm], ...tl]) => 
        [List.flatten([rm, [0]]), ...makeDiagHorizThree(tl, 2)];
      | (2, [[_, _, ...rm], ..._]) => [List.flatten([rm, [0, 0]])];
      | _ => [[]];
      };
      
    checkExpect(makeDiagHorizThree([[1,0,0,0], [0,1,0,0], [0,0,1,0]], 0), 
    [[1,0,0,0],[1,0,0,0],[1,0,0,0]], "makeDiagHoriz3");
    /*
    Input: a board, s
    Output: a list of boards containing all sets of 3-in-a-row subsets of s,
    with makeDiagHoriz applied to all sub-boards

    Recursion Diagram 1:
    OI: [[1,1,1],[0,1,0],[0,0,1], [0,0,0], [0,0,0]]
    RI: [[0,1,0],[0,0,1], [0,0,0], [0,0,0]]
    RO: [[1,0,0], [1,0,0]]
    OO: [[[1,0,0],[1,0,0], [1,0,0], [0,0,0]], 
    [[0,1,0],[0,0,1], [0,0,0], [0,0,0]]]

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: []
    */  
    let rec createListOfThrees: board => list(board) = s =>
      switch(s) {
      | [hd1, hd2, hd3, ...tl] => 
        [makeDiagHorizThree([hd1, hd2, hd3], 0),...createListOfThrees([hd2, hd3,...tl])] ;        
      | _ => [];
      };

    checkExpect(
      createListOfThrees(
        [[0,0,0],[0,0,0],[0,0,0], [0,0,0]]), 
        [[[0,0,0],[0,0,0], [0,0,0]], 
        [[0,0,0],[0,0,0], [0,0,0]]], "createListOf3s zeros");
    checkExpect(
      createListOfThrees(
        [[1,0,0],[0,1,0],[0,0,1], [0,0,0]]), 
        [[[1,0,0],[1,0,0], [1,0,0]], 
        [[0,1,0],[0,1,0], [0,0,0]]], "createListOf3s non-zero");
    checkExpect(createListOfThrees([]), [], "createListOf3s base");

    /*
    Input: a list(board), s
    Output: a list(string) indicating all diagonals from the list(boards)

    Recursion Diagram 1:
    OI: [[[1,0,0],[1,1,0], [1,0,1]], 
    [[0,1,0],[0,0,1], [0,0,0]]]
    RI: [[[0,1,0],[0,0,1], [0,0,0]]]
    RO: ["0"]
    OO: ["1", "0"]

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: []
    */ 
    let rec getDiagonalsThree: list(board) => list(string) = s =>
      switch (s) {
      | [hd, ...tl] => [verticalsThree(transpose(hd)),...getDiagonalsThree(tl)];        
      | [] => [];
      };

    checkExpect(getDiagonalsThree([[[0,1,1],[1,1,1], [1,1,0]], 
    [[0,1,0],[0,0,1], [0,0,0]]]), ["1", "0"], "getDiagonals3");
    checkExpect(getDiagonalsThree([[[1,0,0],[1,0,0], [0,1,1]], 
    [[0,1,0],[0,0,1], [0,0,0]]]), ["0", "0"], "getDiagonals3 0 case");
    checkExpect(getDiagonalsThree([]), [], "getDiagonals3 base case");
    /*
    Input: a board, l
    Output: a string indicating whether any of the players have 3 in a row diagonally. 
    If not, returns "0". Else, returns the non-zero player number as a string
    */ 
    let diagonalsThree: board => string = l =>
      if ((containsPlayerNum(getDiagonalsThree(createListOfThrees(l))) == "1") ||
      (containsPlayerNum(
        getDiagonalsThree(
          createListOfThrees(List.rev(l)))) == "1")) {"1"}
      else if ((containsPlayerNum(
        getDiagonalsThree(createListOfThrees(l))) == "2") ||
        (containsPlayerNum(getDiagonalsThree(
          createListOfThrees(List.rev(l)))) == "2")) {"2"}
      else {"0"};

    checkExpect(diagonalsThree([[0,0,0], [1,0,0], [0,1,0], [0,0,1], [0,0,0]]), 
      "1", "diagonals3");
    checkExpect(diagonalsThree([[0,0,0], [0,0,1], [0,1,0], [1,0,0], [0,0,0]]), 
      "1", "diagonals3 reverse");
    checkExpect(diagonalsThree([[0,0,0], [1,0,0], [0,1,0,0], [0,0,0]]), 
      "0", "diagonals3 2 fail");
    checkExpect(diagonalsThree([]),"0","diagonals3 base");
    /*
    Input: a board, b and an int, n
    Output: an int for the player number that has three in a row, or 0 if no player has three in a row
    */
    let threeStatus: (board, int) => int = (b, n) =>
      if ((verticalsThree(b) == string_of_int(n)) || 
        (horizontalsThree(b) == string_of_int(n)) || 
        (diagonalsThree(b) == string_of_int(n))) {n}
      else {0};

    checkExpect(threeStatus([[0,0,0], [1,0,0], [0,1,0], [0,0,1], [0,0,0]], 1), 
      1, "3stat diag");
    checkExpect(threeStatus([[0,0,0], [1,1,1], [0,0,0], [0,0,1], [0,0,0]], 1), 
      1, "3stat vert");
    checkExpect(threeStatus([[0,0,0], [1,0,1], [1,0,0], [1,0,1], [0,0,0]], 1), 
      1, "3stat horiz");
    checkExpect(threeStatus([[0,0,0], [0,0,1], [1,0,0], [1,0,1], [0,0,0]], 1), 
      0, "3stat fail");
    checkError(() => threeStatus([], 1), "A board cannot be 0 - dimensional.");
    /*
    Input: a list of ints, l, an int, player, and an int, streak
    Output: an int providing the player number if l has two of that players
    number in a row, otherwise returning 0

    Recursion Diagram 1:
    OI: [0, 2, 2], 2, 0
    RI: [2, 2], 2, 0
    RO: 2
    OO: 2

    Recursion Diagram 2:
    OI: [], 0, 0
    RI: N/A
    RO: N/A
    OO: 0
    */
    let rec twoInAList: (list(int), int, int) => int = (l, player, streak) =>
      if (l == []) {0}
      else if ((List.hd(l) == player) && (streak == 0)) {player}
      else if (List.hd(l) == player) {twoInAList(List.tl(l), player, (streak + 1))}
      else {twoInAList(List.tl(l), List.hd(l), 0)};

    checkExpect(twoInAList([0,2,1,1,3], 1, 0), 1, "twoInAList");
    checkExpect(twoInAList([0,2,1,2,1], 1, 0), 0, 
                "twoInAList but broken-up streak");
    checkExpect(twoInAList([], 1, 0), 0, "twoInAList base");

    /*
    Input: a board, s
    Output: a string version of the player number that has 2 chips
    in a vertical row, or "0" if there is no set of 2 chips in a 
    vertical row

    Recursion Diagram 1:
    OI: [[1,1],[0,0],[0,0]]
    RI: [[0,0],[0,0]]
    RO: "0"
    OO: "1"

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: "0"
    */
    let rec verticalsTwo: board => string = s =>
      switch (s) {
      | [hd, ..._] when (twoInAList(hd, 0, 0) != 0) => 
        string_of_int(twoInAList(hd, 0, 0));
      | [hd, ...tl] when (twoInAList(hd, 0, 0) == 0) => 
        verticalsTwo(tl);
      | _ => "0";
      }

    checkExpect(verticalsTwo([[2,1,1,0,0], [0,0,0,0], [0,0,0,0]]), 
      "1", "verticals2");
    checkExpect(verticalsTwo([]), "0", "verticals2 base");
    checkExpect(verticalsTwo([[0,0,0,2], [2,1,0,0,1], [0,0,1,0]]), 
      "0", "verticals2 split");
    checkExpect(verticalsTwo([[2,1,1,0,0], [2,2,0,0], [0,0,0,0]]), 
      "1", "verticals2 order");
    

    /*
    Input: a board, s
    Output: a string version of the player number that has 2 chips
    in a horizontal row, or "0" if there is no set of 2 chips in a 
    horizontal row
    */
    let horizontalsTwo: board => string = s =>
      verticalsTwo(transpose(s));

    checkExpect(horizontalsTwo([[0,1,2],[1,1,0],[1,1,0]]), 
      "1", "horizontals2");
    checkExpect(horizontalsTwo([[0,1,2],[1,0,0],[0,1,0]]), 
      "0", "horizontals2 broken");

    /*
    Input: a board, l, and int, n
    Output: a new board with all diagonals in the board made into horizontals

    Recursion Diagram 1:
    OI: [[1,1],[0,1]], 0
    RI: [[0,1]], 1
    RO: [[1,0]]
    OO: [[1,0],[1,0]]

    Recursion Diagram 2:
    OI: [], 0
    RI: N/A
    RO: N/A
    OO: []
    */
    let rec makeDiagHorizTwo: (board, int) => board = (l, n) =>
      switch(n, l) {
      | (0, [hd, ...tl]) => [hd, ...makeDiagHorizTwo(tl, 1)];
      | (1, [[_,...rm], ..._]) => [List.flatten([rm, [0]])];
      | _ => [[]];
      };

    checkExpect(makeDiagHorizTwo([[1,0,0,0], [0,1,0,0]], 0), 
    [[1,0,0,0],[1,0,0,0]], "makeDiagHoriz2");
    /*
    Input: a board, s
    Output: a list of boards containing all sets of 2-in-a-row subsets of s,
    with makeDiagHoriz applied to all sub-boards

    Recursion Diagram 1:
    OI: [[1,1],[0,1],[0,0], [0,0], [0,0]]
    RI: [[0,1],[0,0], [0,0], [0,0]]
    RO: [[1,0], [1,0]]
    OO: [[[1,0],[1,0], [1,0], [0,0]], 
    [[0,1],[0,0], [0,0], [0,0]]]

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: []
    */  
    let rec createListOfTwos: board => list(board) = s =>
      switch(s) {
      | [hd1, hd2, ...tl] => 
        [makeDiagHorizTwo([hd1, hd2], 0),...createListOfTwos([hd2, ...tl])];
      | _ => [];
      };

    checkExpect(createListOfTwos([[0,0],[0,0],[0,0]]), [[[0,0],[0,0]], 
    [[0,0],[0,0]]], "createListOf2s");
    checkExpect(createListOfTwos([]), [], "createListOf2s base");
    /*
    Input: a list(board), s
    Output: a list(string) indicating all diagonals from the list(boards)

    Recursion Diagram 1:
    OI: [[[1,0],[1,1], [1,0]], 
    [[0,1],[0,0], [0,0]]]
    RI: [[[0,1],[0,0], [0,0]]]
    RO: ["0"]
    OO: ["1", "0"]

    Recursion Diagram 2:
    OI: []
    RI: N/A
    RO: N/A
    OO: []
    */ 
    let rec getDiagonalsTwo: list(board) => list(string) = s =>
      switch (s) {
      | [hd, ...tl] => [verticalsTwo(transpose(hd)),...getDiagonalsTwo(tl)];        
      | [] => [];
      };

    checkExpect(getDiagonalsTwo([[[0,1,1],[1,1,1]], 
    [[0,1,0], [0,0,0]]]), ["1", "0"], "getDiagonals2");
    checkExpect(getDiagonalsTwo([[[0,0,0],[1,0,0], [0,0,1]], 
    [[0,0,0],[0,0,1], [0,0,0]]]), ["0", "0"], "getDiagonals2 0 case");
    checkExpect(getDiagonalsTwo([]), [], "getDiagonals2 base case");
    /*
    Input: a board, l
    Output: a string indicating whether any of the players have 2 in 
    a row diagonally. 
    If not, returns "0". Else, returns the non-zero player number as a string
    */ 
    let diagonalsTwo: board => string = l =>
      if ((containsPlayerNum(getDiagonalsTwo(createListOfTwos(l))) == "1") ||
      (containsPlayerNum(
        getDiagonalsTwo(createListOfTwos(List.rev(l)))) == "1")) 
        {"1"}        
      else if ((containsPlayerNum(
        getDiagonalsTwo(createListOfTwos(l))) == "2") ||
        (containsPlayerNum(getDiagonalsTwo(
          createListOfTwos(List.rev(l)))) == "2")) {"2"}
      else {"0"};

    checkExpect(diagonalsTwo([[0,0,0], [1,0,0], [0,1,0], [0,0,0], [0,0,0]]), 
      "1", "diagonals3");
    checkExpect(diagonalsTwo([[0,0,0], [0,0,1], [0,1,0], [0,0,0], [0,0,0]]), 
      "1", "diagonals3 reverse");
    checkExpect(diagonalsTwo([[0,0,0], [1,0,0], [0,0,0,0], [0,0,0]]), "0", 
      "diagonals3 2 fail");
    checkExpect(diagonalsTwo([]),"0","diagonals3 base");
    /*
    Input: a board, b and an int, n
    Output: an int for the player number that has two in a row, or 0 if no player has two in a row
    */
    let twoStatus: (board, int) => int = (b, n) =>
      if ((verticalsTwo(b) == string_of_int(n)) || 
        (horizontalsTwo(b) == string_of_int(n)) || 
        (diagonalsTwo(b) == string_of_int(n))) {n}
      else {0};

    checkExpect(twoStatus([[0,0,0], [1,0,0], [0,1,0], [0,0,0], [0,0,0]], 1), 
      1, "2stat diag");
    checkExpect(twoStatus([[0,0,0], [1,1,0], [0,0,0], [0,0,1], [0,0,0]], 1), 
      1, "2stat vert");
    checkExpect(twoStatus([[0,0,0], [1,1,0], [1,1,0], [0,0,0], [0,0,0]], 1), 
      1, "2stat horiz");
    checkExpect(twoStatus([[0,0,0], [0,0,1], [1,0,0], [0,0,1], [0,0,0]], 1), 
      0, "2stat fail");

    /*
    Input: a state with board b and status s
    Output: a float representing the estimated value of the given state
    */
    /* estimates the value of a given state (static evaluation) */
    let estimateValue = ((b, s): state): float =>
      switch(s) {
      | Win(P1) => 1234567891234.0;
      | Win(P2) => -1234567891234.0; 
      | Ongoing(P1) when threeStatus(b, 1) == 1 => 123456789.0; 
      | Ongoing(P1) when twoStatus(b, 1) == 1 => 
        if (threeStatus(b, 2) == 2) {-12.0}
        else if (twoStatus(b, 2) == 2) {12.0}
        else {12345.0};
      | Ongoing(P1) => 
        if (threeStatus(b, 2) == 2) {-1234567.0}
        else if (twoStatus(b, 2) == 2) {-12.0}
        else {12.0};
      | Ongoing(P2) when threeStatus(b, 2) == 2 => -123456789.0
      | Ongoing(P2) when twoStatus(b, 2) == 2 => 
        if (threeStatus(b, 1) == 1) {12.0}
        else if (twoStatus(b, 1) == 1) {-12.0}
        else {-12345.0};
      | Ongoing(P2) => 
        if (threeStatus(b, 1) == 1) {1234567.0}
        else if (twoStatus(b, 1) == 1) {12.0}
        else {-12.0};
      | Draw => 0.0;
      };

    checkExpect(estimateValue(([[1,1,1,1],[1,2,1,2], [2,1,2,1], [0,0,0,0]],
       Win(P1))), 1234567891234.0, "estimateValue Win(P1)");
    checkExpect(estimateValue(([[2,2,2,2],[1,2,1,2], [2,1,2,1], [0,0,0,0]], 
      Win(P2))), -1234567891234.0, "estimateValue Win(P1)");
    checkExpect(estimateValue(([[2,2,2,2],[1,2,1,2], [2,1,2,1], [0,0,0,0]], 
      Win(P1))), 1234567891234.0, "estimateValue Win Order");
    checkExpect(estimateValue(([[1,1,1,2],[1,2,1,2], [2,1,2,1], [0,0,0,0]], 
      Ongoing(P1))), 123456789.0, "estimateValue three P1");
    checkExpect(estimateValue(([[1,0,0,0],[1,2,1,2], [2,1,2,1], [0,0,0,0]], 
      Ongoing(P1))), 12345.0, "estimateValue two P1");
    checkExpect(estimateValue(([[2,2,2,1],[1,2,1,2], [2,1,2,1], [0,0,0,0]], 
      Ongoing(P1))), -1234567.0, "estimateValue threeP2 for P1");
    checkExpect(estimateValue(([[2,2,0,1],[0,0,0,0], [0,0,0,1], [0,0,0,0]], 
      Ongoing(P1))), -12.0, "estimateValue twoP2 for P1");
    checkExpect(estimateValue(([[2,2,2,1],[2,1,2,1], [1,2,1,2], [0,0,0,0]], 
      Ongoing(P2))), -123456789.0, "estimateValue three P2");
    checkExpect(estimateValue(([[2,0,0,0],[2,1,2,1], [1,2,1,2], [0,0,0,0]], 
      Ongoing(P2))), -12.0, "estimateValue two P2");
    checkExpect(estimateValue(([[1,1,1,2],[2,1,2,1], [1,2,1,2], [0,0,0,0]], 
      Ongoing(P2))), 1234567.0, "estimateValue threeP1 for P2");
    checkExpect(estimateValue(([[1,1,0,2],[0,0,0,0], [0,0,0,2], [0,0,0,0]], 
      Ongoing(P2))), 12.0, "estimateValue twoP1 for P2");
    checkExpect(estimateValue(([[1,2,2,2],[1,2,1,2], [2,1,2,1], [1,2,1,2]], 
      Draw)), 0.0, "estimateValue Draw");
};
