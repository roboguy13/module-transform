# Building

    $ cabal install --only-dependencies
    $ cabal build

# Running on example

The following will generate a GraphViz graph and then render this to a file
named `editor-example.png`:

    $ cabal -v0 run module-graph Main.java ../java-examples/editor | sfdp -x -Goverlap=scale -Tpng > editor-example.png

(Note that the `-v0` flag prevents `cabal run` from printing unnecessary build
status output.)

# General command line argument format for `module-graph`

    $ cabal -v0 run module-graph <module file to start with> <path 1> <path 2> ... <path N>

where `<path 1>` through `<path N>` are the names of directories for the
relevant Java module search paths.

