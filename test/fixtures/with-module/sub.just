# Pipeline root: discovered via [metadata("ci")] inside a submodule.
[metadata("ci")]
entry: a b
    echo entry

# Two parallel siblings, both depending on a shared upstream — exercises
# qualification of an unqualified dep that refers to a submodule sibling.
[parallel]
fan: a b
    echo fan

a: shared
    echo a

b: shared
    echo b

shared:
    echo shared
