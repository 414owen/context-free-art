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

## Example

```haskell
import Art.ContextFree
import Data.List.NonEmpty

-- let's define a Production rule
a = Circle 1

-- this will produce an IO Svg from the blaze-svg package
-- to turn it into a string we can use one of the `blaze-svg` renderers
graphic1 = interpret $ Circle 1

-- let's create a non-terminal, at every layer,
-- this will have an 85% chance of rendering another circle
a = Mod [Move (2, 0)] b
b = NonTerminal $ (85, c) :| []
c = NonTerminal $ (100, Circle 1) :| [(100, a)]
```
