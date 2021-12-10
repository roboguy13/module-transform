# Requirements

- GHC
- Cabal
- GraphViz

# Building

    $ cabal install --only-dependencies
    $ cabal build

# Running on example

The following will generate a DOT graph and then render this to a file
named `editor-example.png`:

    $ cabal -v0 run module-graph Main ../java-examples/editor | sfdp -x -Goverlap=scale -Tpng > editor-example.png

(Note that the `-v0` flag prevents `cabal run` from printing unnecessary build
status output to stdout.)

# General command line argument format for `module-graph`

    $ cabal -v0 run module-graph <module name to start with> <path 1> <path 2> ... <path N>

where `<path 1>` through `<path N>` are the names of directories for the
relevant Java module search paths.

This will write a module graph in the DOT graph language, which can then be
rendered using GraphViz tools such as `sfdp`.

