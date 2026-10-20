#!/bin/sh
set -ex

# Run the bootstrapping interpreter on Ouroboros
ocamlopt -w -a -o bootstrap bootstrap.ml
./bootstrap ouroboros.ob > ouroboros.ll

# Assemble and link
llc -filetype=obj ouroboros.ll -o ouroboros.o
cc ouroboros.o alloc.c trie.c util.c misc.c -o ouroboros

# Run the Ouroboros interpreter on itself
./ouroboros ouroboros.ob > ouroboros2.ll

# Check that it outputs its own LLVM assembly
diff ouroboros.ll ouroboros2.ll