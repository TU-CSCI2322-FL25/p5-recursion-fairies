Recursion Fairies

Functionality (61+4=65/73 points)
* Game mechanics:                                 18/20 points
   * Making a move doesn’t change the next forced move properly - it should use the cellLoc, not sbLoc. You KNOW what the board is at sbLoc, you wouldn’t need to split again.
* Exact game solver:                                 12/15 points
   * BestMove doesn’t consider who won when computing priorities!
* Cut-off depth solver:                                 12 points
* Reasonable evaluation function:                 1/2 points
   * Rated by current player, which is problematic when the current player doesn’t change on won games.
* Avoiding unnecessary work:                            2/3 points
   * Extra not avoiding: the use of minimumBy in choosePlayerBest robs you of this for whoWillWin. chooseBestLazy should work, but needing to negate is a symptom of your rating functions.
* Command-line interface:                         9/10 points
   * Doesn’t print help on most errors, because of clunky main.
* Move and verbose flags:                         4/5 points
   * parenthesis in move format
* Error-handling:                                 2/5 points
   * spotToPLayer is unsafe and actually used.
   * placeSpot should probably not return an unchanged board if the piece is full.
   * error handling in makeMove, although avoidably nested cases.
   * fromJust in bestMove.
* Makefile:                                        1/1 point
   * makes, but still called Main
* Interactive:                                        4/5
   * Better description of move formatting would help, but very nice

Design (21/27 points)
* Well-designed data types                        8 points
* Well-decomposed functions                        6/10 points
   * Some dead-code with questionable functions (isPlayerX, canWin)
   * updateSupBoard and WinnerOFBoard share significant overlap in code, at least some of which could have been abstracted out.
   * Main is in desperate need of some decomposition.
* Good module decomposition                        2 points
* Good variable names                                2 points
* Efficient/idiomatic code                        3/5 points
   * Definitely some clunky code.
