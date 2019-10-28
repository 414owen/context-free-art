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

## Examples

```haskell
import Art.ContextFree.Probabilistic
import Data.List.NonEmpty

-- let's define a Production rule
a = Circle 1

-- this will produce an IO (Maybe Svg) from the blaze-svg package
-- to turn it into a string we can use one of the `blaze-svg` renderers
graphic1 = render $ Circle 1

-- let's create a non-terminal, 'a', which renders a terminal, 'Circle 1'
-- and has an 85% chance of rendering itself, placed to its right
a = NonTerminal $ (100, Circle 1) :| [(85, b)]
b = Mod [Move (2, 0)] a
```

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
