# Haskell to Python FFI 

Compiles a haskell program with many external dependencies into a .dll file callable from python using the msgpack package.

The yaml/cabal files and Main.def need to be modified when switching the source files. Serialisation of Haskell functions is handled using a very slightly modified version (so it works on my machine) of the call-haskell-from-anything package from hackage. The Haskell source files must import this and serialise the functions to be called through FFI.
