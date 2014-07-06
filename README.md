Haster!
========================
  
Twitter like app in Haskell using Happstack and IxSet

Instructions
==============

`export PATH=~/.cabal/bin:$PATH`

`cabal update`

`cabal install happstack-server`

`cabal install ixset`

`cabal install acid-state`

Compile it: `ghc -threaded server.hs -o server`

Run it: `./server`
