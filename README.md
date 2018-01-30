# maze

# About

 A program capable of creating and solving Perfect Mazes and Braid Mazes built
 on the principles of functional programming. It was created as a project for
 the **Principles of Programming Languages** course, in the Fall semester of
 2016-2017 by **Agapiou Marinos** and **Kamaras Georgios**.

# Sample run

```
Prelude> :l maze.hs
[1 of 1] Compiling Main
( maze.hs, interpreted )
Ok, modules loaded: Main.
*Main> let m = makeMaze 5 5
*Main> let pm = kruskal m
*Main> let ps = solvePerfect pm (2,3) (5,4)
*Main> let pp = showMaze pm ps
*Main> putStr pp
<perfect maze's matrix>
*Main> let bm = braid pm
*Main> let bs = solveBraid bm (2,3) (5,4)
*Main> let pb = showMaze bm bs
*Main> putStr pb
<braid maze's matrix>
```
