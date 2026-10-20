# Ouroboros

Ouroboros is an interpreter for a toy scripting language, and a compiler for a
toy systems language, both of which have Lisp-like syntax.  What makes it unique
is that both languages are defined in the same file:  The first part of the
file (before the `___stage2___` line) is written in the scripting language, and
implements the compiler, and the second part of the file (after the
`___stage2___` line) is written in the systems language and implements the
scripting language.  They implement each other, like an ouroboros!

Since you cannot run ouroboros without already having an implementation of the
scripting language, the scripting language is also implemented in OCaml in
`bootstrap.ml`.  This implementation should be identical to the one implemented
in `ouroboros.ob`, and you can check that the output of `ouroboros.ob` is the
same when it is run with the bootstrapper or with ouroboros itself by running
`check.sh`.

## Notes

- The compiler outputs LLVM assembly.
- ◎ is null.
- ⊤ is true.
- ⊥ is false.
- ʹ is the unquote operator (like comma in most Lisps) unless it is after an
  identifier, in which case it is part of the identifier (like how prime is used
  in math).
- ʺ is similar except that it is unquote-splicing (like `,@` in Lisp) if used as
  an operator.
- Pattern match on structure types using `(# <Type> <pattern1> <pattern2>)`.
- `.` is a pattern that matches anything without binding it to a name.
- `(@ <name> <pattern>)` is a pattern which binds `name` to the value, and also
  does pattern-matching using `pattern`.

## Dependencies

OCaml for bootstrapping, a C compiler for a few supplemental functions not
implemented in ouroboros itself, and LLVM to compile the output of the
compiler. On Ubuntu:

```
sudo apt install ocaml llvm build-essential
```

## Usage

```
./check.sh
```

The script is really simple, so look inside to see how to compile and run
ouroboros.
