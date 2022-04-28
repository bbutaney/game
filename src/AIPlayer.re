open! CS17SetupGame;
open Game; 

/* Note: Some data definitions may be repeated from other modules.
 I have included all of the data definitions and example data
 used in this module in the comment directly below this one. */

/*
   AIPlayer.re Data Definitions:
 
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
 
   A list(float) is either
       [item, ...b] where item is a float and b is a list(float), or
       empty
 
   A list(state) is either
       [item, ...b] where item is a state and b is a list(state), or
       empty
 
   AIPlayer.re Example Data:

   board: [[0, 0, 2, 1], [0, 0, 2, 1], [0, 2, 1, 1], [0, 2, 2, 1]],
          [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
  
   status: Ongoing(P1), Win, Draw, Ongoing(P2)
 
   whichPlayer: P1, P2
 
   state: 
   (Ongoing(P1), [[0, 0, 2, 1], [0, 0, 2, 1], [0, 2, 1, 1], [0, 2, 2, 1]]), 
   (Ongoing(P1), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]])
  
   move: Move(1), Move(4)
 
   list(move): [Move(1), Move(4)], [Move(3)]
 
   int: 1, 2

   float: 12.5, 0.0
 
   list(float): [1.0, 1.5], [0.0, 6.6]
 
   list(state): [(Ongoing(P1), [[0, 0, 2, 1], [0, 0, 2, 1], 
   [0, 2, 1, 1], [0, 2, 2, 1]]), (Ongoing(P1), [[0, 0, 0, 0], [0, 0, 0, 0], 
   [0, 0, 0, 0], [0, 0, 0, 0]])], []
*/



module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame

    /*
      Input: a list(move), l, and a state, s
      Output: a list(float) giving the estimate value of every move in l when
      applied to the state s

      Recursion Diagram 1:
      OI:[Move(1), Move(2)], ([[0,0,0], [0,0,0], [0,0,0]], Ongoing(P1))
      RI: [Move(2)], ([[0,0,0], [0,0,0], [0,0,0]], Ongoing(P1))
      RO: [12.0]
      OO: [12.0, 12.0]

      Recursion Diagram 2:
      OI: [], ([], Ongoing(P1))
      RI: N/A
      RO: N/A
      OO: []
    */  
    let rec createValueList: (list(PlayerGame.move), PlayerGame.state) => 
    list(float) = (l, s) =>
      switch (l) {
      | [] => [];
      | [hd, ...tl] => [PlayerGame.estimateValue(
        PlayerGame.nextState(s, hd)), ... createValueList(tl, s)];
      };

    /*
      Input: a list(float), l, and a float, max
      Output: the maximum float in l

      Recursion Diagram 1:
      OI:[10.0, 2.0, 3.0]
      RI: [2.0, 3.0]
      RO: [3.0]
      OO: [10.0]

      Recursion Diagram 2:
      OI: []
      RI: N/A
      RO: N/A
      OO: []
    */ 
    let rec findMax: (list(float), float) => float = (l, max) =>
      switch(l) {
      | [] => max;
      | [hd, ...tl] when max < hd => findMax(tl, hd);
      | [_, ...tl] => findMax(tl, max);
      }
    checkExpect(findMax([1.0, 10.0, 1000.0, 9.0, 1.0], 0.0), 1000.0, 
      "testing findMax");
    checkExpect(findMax([], 0.0), 0.0, "testing findMax base case");
      
    /*
      Input: a state, s, and a list(move), l
      Output: a list of states with every move in l applied to s and returned
      as a new state in the list

      Recursion Diagram 1:
      OI:([[0,0,0], [0,0,0], [0,0,0]], Ongoing(P1)), [Move(1), Move(2)]
      RI: ([[0,0,0], [0,0,0], [0,0,0]], Ongoing(P1)), [Move(2)]
      RO: ([[0,0,1], [0,0,0], [0,0,0]], Ongoing(P1))
      OO: [([[0,0,], [0,0,1], [0,0,0]], Ongoing(P2)), 
      ([[0,0,1], [0,0,0], [0,0,0]], Ongoing(P2))]

      Recursion Diagram 2:
      OI: ([], Ongoing(P1)), []
      RI: N/A
      RO: N/A
      OO: []
    */ 
    let rec allNextStates: (PlayerGame.state, list(PlayerGame.move)) => 
    list(PlayerGame.state) = (s, l) =>
      switch(l) {
      | [] => [];
      | [hd, ...tl] when 
        PlayerGame.gameStatus(PlayerGame.nextState(s, hd)) == Win(P1) 
        => [PlayerGame.nextState(s, hd), ... allNextStates(s, tl)];
      | [hd, ...tl] => [PlayerGame.nextState(s, hd), ... allNextStates(s, tl)];
      }

    /*
      Input: a list(state), l
      Output: a list(state) that gets all legal next states for 
      every next state in l

      Recursion Diagram 1:
      OI:[([[1,1,1], [0,2,2], [0,0,2]], Ongoing(P1))]
      RI: [([[0,2,2], [0,0,2]], Ongoing(P1))]
      RO: [([[1,2,2], [0,0,2]], Ongoing(P1)), ([[0,2,2], [0,1,2]], Ongoing(P1))]
      OO: [([[1,1,1], [1,2,2], [0,0,2]], Ongoing(P1)), 
      ([[1,1,1], [0,2,2], [0,1,2]], Ongoing(P1))]

      Recursion Diagram 2:
      OI: []
      RI: N/A
      RO: N/A
      OO: []
    */ 
    let rec getDepthBelow: list(PlayerGame.state) => 
    list(PlayerGame.state) = l =>
      switch(l) {
      | [] => [];
      | [hd, ...tl] => List.flatten(
        [allNextStates(hd, PlayerGame.legalMoves(hd)), getDepthBelow(tl)])
      }

    /*
      Input: an int, depth, a state, s, and a list(state), l
      Output: a list(state) that gets all legal next states at the lowest
      depth of the minimax tree

      Recursion Diagram 1:
      OI: 2, [([[1,1,1], [0,2,2], [0,1,2]], Ongoing(P2))], []
      RI: 2, [([[0,2,2], [0,1,2]], Ongoing(P2))], []
      RO: [([[1,2,2], [2,1,2]], Ongoing(P1)), ([[2,2,2], [1,1,2]], Ongoing(P1))]
      OO: [([[1,1,1], [1,2,2], [2,1,2]], Ongoing(P1)), 
      ([[1,1,1], [2,2,2], [1,1,2]], Ongoing(P1))]

      Recursion Diagram 2:
      OI: 0, [], []
      RI: N/A
      RO: N/A
      OO: []
    */ 
    let rec getDepthZero: (int, PlayerGame.state, list(PlayerGame.state)) => 
    list(PlayerGame.state) = (depth, s, l) =>
      switch(depth, l) {
      | (0, _) => l;
      | (_, []) => getDepthZero((depth - 1), s, getDepthBelow([s]));
      | (_, _) => 
        getDepthZero((depth - 1), s, List.flatten([getDepthBelow(l)]));
      };

    /*
      Input: a state, max, and a list(state), l
      Output: the best state in l

      Recursion Diagram 1:
      OI: [([[1,1,1], [0,2,2], [0,1,2]], Ongoing(P2)), 
      ([[2,2,2], [0,0,0], [2,1,1]], Ongoing(P2))]
      RI: [([[2,2,2], [0,0,0], [2,1,1]], Ongoing(P2))]
      RO: ([[2,2,2], [0,0,0], [2,1,1]], Ongoing(P2))
      OO: ([[1,1,1], [0,2,2], [0,1,2]], Ongoing(P2))

      Recursion Diagram 2:
      OI: ([], Ongoing(P1)), []
      RI: N/A
      RO: N/A
      OO: ([], Ongoing(P1))
    */ 
    let rec findBestState: (list(PlayerGame.state), PlayerGame.state) => 
      PlayerGame.state = (l, max) =>
      switch(l) {
      | [] => max;
      | [hd, ...tl] when PlayerGame.estimateValue(max) < 
        PlayerGame.estimateValue(hd) => findBestState(tl, hd);
      | [_, ...tl] => findBestState(tl, max);
      }

    /*
      Input: a state, s, an int, depth, and a move, m
      Output: the best state at depth zero given 
      a state, depth, and a move
    */ 
    let getBestFromMove: (PlayerGame.state, int, PlayerGame.move) => 
      PlayerGame.state = (s, depth, mv) =>
      findBestState(getDepthZero(depth, PlayerGame.nextState(s, mv), []), s);

    /*
      Input: a state, s, and a list(move), legals, a move, bestM,
      a status, p, and an int, depth
      Output: the best legal move from legals

      Recursion Diagram 1:
      OI: [([[1,1,1], [0,2,2], [0,1,2]], Ongoing(P2)), [Move(1), Move(2)],
      Move(1), Ongoing(P2), 1
      RI: [([[1,1,1], [0,2,2], [0,1,2]], Ongoing(P2))], [Move(2)],
      Move(1), Ongoing(P2), 1
      RO: Move(2)
      OO: Move(1)

      Recursion Diagram 2:
      OI: ([], Ongoing(P1)), [], Move(0), Ongoing(P1), 0
      RI: N/A
      RO: N/A
      OO: Move(0)
    */ 
    let rec pickBestLegalMove: 
    (PlayerGame.state, list(PlayerGame.move), 
    PlayerGame.move, PlayerGame.status, int) => 
    PlayerGame.move = (s, legals, bestM, p, depth) => 
      switch (legals, p) {
      | ([], _) => bestM;
      | ([hd, ...tl], Ongoing(P2)) 
        when PlayerGame.estimateValue(getBestFromMove(s, depth, hd)) < 
        PlayerGame.estimateValue(getBestFromMove(s, depth, bestM)) => 
        pickBestLegalMove(s, tl, hd, p, depth);
      | ([hd, ...tl], Ongoing(P2)) 
        when PlayerGame.estimateValue(getBestFromMove(s, depth, hd)) > 
        PlayerGame.estimateValue(getBestFromMove(s, depth, bestM)) => 
        pickBestLegalMove(s, tl, bestM, p, depth);
      | ([hd, ...tl], Ongoing(P1)) 
        when PlayerGame.estimateValue(getBestFromMove(s, depth, hd)) > 
        PlayerGame.estimateValue(getBestFromMove(s, depth, bestM)) => 
        pickBestLegalMove(s, tl, hd, p, depth);
      | ([hd, ...tl], Ongoing(P1)) 
        when PlayerGame.estimateValue(getBestFromMove(s, 2, hd)) < 
        PlayerGame.estimateValue(getBestFromMove(s, depth, bestM)) => 
        pickBestLegalMove(s, tl, bestM, p, depth);
      | ([hd, ...tl], _) => pickBestLegalMove(s, tl, hd, p, depth);
      }

    /*
    Input: a state, s
    Output: the best next move given the inputted state
    */
    let nextMove = ((s): PlayerGame.state): PlayerGame.move =>
      pickBestLegalMove(s, PlayerGame.legalMoves(s), 
      List.hd(PlayerGame.legalMoves(s)), PlayerGame.gameStatus(s), 3);

      // let nextMove = ((s): PlayerGame.state): PlayerGame.move =>
      // pickBestLegalMove(s, PlayerGame.legalMoves(s), 
      // List.hd(PlayerGame.legalMoves(s)));
    

    /* put your team name here! */
    let playerName = "";
  
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame); 
module MyAIPlayer:Player = TestAIPlayer;

open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */
checkExpect(findMax([1.0, 10.0, 1000.0, 9.0, 1.0], 0.0), 1000.0, 
  "testing findMax");
checkExpect(findMax([], 0.0), 0.0, "testing findMax base case");