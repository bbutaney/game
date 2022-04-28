/* Note: Some data definitions may be repeated from other modules.
 I have included all of the data definitions and example data
 used in this module in the comment directly below this one. */

/*
    Game Data Definitions:

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

    Game.re Example Data

    board: [[0, 0, 2, 1], [0, 0, 2, 1], [0, 2, 1, 1], [0, 2, 2, 1]], 
           [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
    
    status: Ongoing(P1), Win, Draw, Ongoing(P2)

    whichPlayer: P1, P2

    state:(Ongoing(P1), 
            [[0, 0, 2, 1], [0, 0, 2, 1], [0, 2, 1, 1], [0, 2, 2, 1]]),
          (Ongoing(P1), 
            [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]])
    
    move: Move(1), Move(4)

    list(move): [Move(1), Move(4)], [Move(3)]

    String: "hi", "bye"

    float: 12.5, 0.0
*/

module type Game = {

    /* specifies a player */
    type whichPlayer = P1 | P2;

    /* status of game: if it's over (and who won) or ongoing (and who's turn) */
    type status =
       | Win(whichPlayer)
       | Draw
       | Ongoing(whichPlayer);

    /* the state of the game: the position, status, anything else associated
    with the game at a given turn */
    type state;

    /* describes a move that a player can make */
    type move;

    /* printing functions */
    /*
      Input: wP, a whichPlayer
      Output: a string representation of wP
    */
    let stringOfPlayer: whichPlayer => string;
    /* 
    Input: a state, with its board being b and its status being unnamed
    Output: a string representation of the board transposed to look like
    a connect four board 
    */
    let stringOfState: state => string;
    /*
    Input: a move, m
    Output: a string representation of the int contained within m
    */
    let stringOfMove: move => string;

    /* Game Logic */

    /* the state of the game when it begins */
    /*
    Input: a string, s, detailing dimensions for a board
    Output: a state with an empty board based on the dimensions specified in s 
    and with the status of the state defaulting to Ongoing(P1)
    */
    let initialState: string => state;

    /* produces the list of legal moves at a state */
    /*
    Input: a state, with its board element b and its status element being any
    possible status value
    Output: the list of legal moves at the inputted state
    */
    let legalMoves: state => list(move);

    /* returns the status of the game at the given state */
    /*
    Input: a state, split into its board, b, and its status, which 
    can be anything
    Output: the status of the game at the inputted state
    */
    let gameStatus: state => status;

    /* given a state and a legal move, yields the next state */
    /*
    Input: a state, with board b and status s, and a move, m
    Output: the next state given b, s, and m
    */
    let nextState: (state, move) => state;

    /* for transforming human player input into
    internal representation of move */
    /*
    Input: a string, input, and a state, s
    Output: a move version of the inputted string
    */
    let moveOfString: (string, state) => move;

    /* estimates the value of a given state (static evaluation) */
    /*
    Input: a state with board b and status s
    Output: a float representing the estimated value of the given state
    */
    let estimateValue: state => float;
};
