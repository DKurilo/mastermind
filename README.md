# mastermind

Here is simple [mastermind game](https://en.wikipedia.org/wiki/Mastermind_(board_game)) implementation I did for [functional programming in Haskell course](https://www.futurelearn.com/courses/functional-programming-haskell/8).  
For computer as codebreaker Knuth algorithm was used. So most probably you have no chance against computer. Sorry. :)

## Algorithm explanation.

Let's think about key pegs like it is distance between codes.  
Now let's suppose we need to find point and that is placed on some distance from all other points.  
That's exactly what we need to do. we need to pick first point, then we have distance and now we just need to find all points that are placed on the same distance from picked point as we have as hint.  
Now we need to pick one more point from from points that we found on the previous step and ask for new distane..
