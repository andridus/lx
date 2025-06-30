-module(ex1).
-export([func/1, func/2]).

func(X) ->
X.
-record(Coordinate, {x, y}).
func(X) ->
X;
func(Coord) ->
X2 = Coord#x * Coord#x,
Y2 = Coord#y * Coord#y,
Math#sqrt(X2).

func(A, B) ->
A + B.
