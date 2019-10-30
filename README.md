# Context free art

Create art via context free grammar production rules.
Current backends: SVG.

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

The latter produces this graphic:

![spiral.svg](https://owen.cafe/res/context-free/spiral.svg)

## Examples

The code for these can be found in the [exmaples/](https://github.com/414owen/context-free-art/tree/master/examples) folder

![circles.svg](https://owen.cafe/res/context-free/circles.svg)
![sierpinski-triangle.svg](https://owen.cafe/res/context-free/sierpinski-triangle.svg)
![sierpinski-carpet.svg](https://owen.cafe/res/context-free/sierpinski-carpet.svg)
