/* take in a string, s, and print it with green color */
let printGreen = (s: string): unit =>
  print_string("\027[32m" ++ s ++ "\027[0m\n");

/* take in a string, s, and print it with red color */
let printRed = (s: string): unit =>
  print_string("\027[31m" ++ s ++ "\027[0m\n");

type result('a) =
  | Actual_Result('a)
  | Expected_Result('a)
  | Actual_Error(string)
  | Expected_Error(string);

type check_result('a) =
  | Test_Passed
  | Test_Failed(result('a), result('a));

/* checkExpect
  Inputs: actual and expected, two 'a and message, a string
  Output: nothing */
let checkExpect = (actual: 'a, expected: 'a, message: string): unit =>
  if (actual == expected) {
    printGreen("checkExpectSuccess: " ++ message);
  } else {
    printRed("checkExpectFail: " ++ message);
    printRed("expected output: ");
    Js.log(expected);
    printRed("actual output: ");
    Js.log(actual)
  };
  
/* checkError
  Input: a one-argument procedure 'input' that you want to test
  and a string of the error message of the 'failwith' clause in the procedure
  Output: nothing */
let checkError = (input: unit => 'a, expect: string): unit =>
  try (
    {
      ignore(input()); // we expect this to error
      failwith("Error did not occur");
    }
  ) {
  | Failure(err) when err == expect =>
    printGreen("checkErrorSuccess");
  | Failure(err) when err == "Error did not occur" =>
    printRed("Error did not occur");
  | Failure(err) =>
    printRed("checkErrorFail. Expected error: " ++ expect ++ "; Actual error: " ++ err);
  };

let rec parseBoardDims: string => list(int) =
  t => switch(String.trim(t)){
  |"" => []
  | v =>
  {
    let s = v ++ " "; /* ensure there's a blank! */
    let len = String.length(s);
    let firstSpace = String.index(s, ' ');    
    let num = String.sub(s, 0, firstSpace);
      let remainder = String.sub(s, firstSpace, len - firstSpace);
      [int_of_string(num),...parseBoardDims(remainder)]
   }
};

let getBoardHeight: list(int) => int =
  dims =>
    switch (dims) {
    | [height, _] => height
    | _ => failwith("invalid dimensions")
    };

let getBoardWidth: list(int) => int =
  dims =>
    switch (dims) {
    | [_, width] => width
    | _ => failwith("invalid dimensions")
    };


/* --------------------------- checkExpectListAlpha ---------------------------
 * Inputs: actual, given value to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and 
 *         expected are the same and fails otherwise
 */
let checkExpectListAlpha: (list('a), list('a), string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      Js.log(Array.of_list(expected));
      printRed("actual output: ");
      Js.log(Array.of_list(actual));
    };

/* ------------------------- checkExpectListListAlpha -------------------------
 * Inputs: actual, given value to be checked against expected  
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and 
 *         expected are the same and fails otherwise
 */
let checkExpectListListAlpha:
  (list(list('a)), list(list('a)), string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      Js.log(Array.of_list(List.map(x => Array.of_list(x), expected)));
      printRed("actual output: ");
      Js.log(Array.of_list(List.map(x => Array.of_list(x), actual)));
    };

