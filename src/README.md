# Motivation
The OpenScad language is, as usual, a messy one, although it has some elements of cleanliness. 
This is an attempt to clean it up and provide a functional way to express CAD. 

# Usage

# Syntax
```
program ::= objects

exprs ::=  (objects expr ...)
         | (transform expr ...)
         | (extrude expr ...)
         | (projection expr ...)
         | (render-with expr ...)
         | (height-map args file expr ...)
         | object


object ::= 2d-object
        |  3d-object
        |  union exprs
        |  intersection expres
        |  difference exprs
        |  hull exprs
        |  minkowski exprs
        |  import file

parameters: 
$fa minimum angle
$fs minimum size
$fn number of fragments
```

