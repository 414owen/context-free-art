# Context free art

Create art via context free grammar production rules.

## Context free grammar primer

Context free grammars consist of a set of terminal symbols, a set of
non-terminal symbols, and production rules that map non-terminals to
other symbols.

With a context-free grammar, we can generate strings of terminals that
conform to the specified language.

Our language will describe graphics.

## How to use

```haskell
import Art.ContextFree.Definite
import Data.List.NonEmpty

move = Mod [Move (0, -1.8), Scale 0.8]

armN :: Int -> Symbol
armN 0 = move $ Circle 1
armN n = move $ Branch $
  Circle 1 :| [Mod [Rotate 10] $ armN (n - 1)]

arm :: Symbol
arm = armN 20

spiral = Branch $
  Circle 1 :| [arm, Mod [Rotate 120] arm, Mod [Rotate 240] arm]
```

This code produces this image:

<img src="https://raw.githubusercontent.com/414owen/context-free-art/master/examples/spiral.svg" width="300" alt="spiral.svg">

## Examples

The code for these can be found in the [exmaples/](https://github.com/414owen/context-free-art/tree/master/examples) folder
<p align="center">
<img src="https://raw.githubusercontent.com/414owen/context-free-art/master/examples/circles.svg" width="300" alt="circles.svg">
<img src="https://raw.githubusercontent.com/414owen/context-free-art/master/examples/Sierpinski/triangle.svg" width="300" alt="sierpinskis-triangle.svg">
<img src="https://raw.githubusercontent.com/414owen/context-free-art/master/examples/Sierpinski/carpet.svg" width="300" alt="sierpinskis-carpet.svg">
</p>
