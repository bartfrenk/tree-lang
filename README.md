# TreeLang

## Description

A (non-Turing complete) language for describing decision trees, with syntax
similar to Python. Loosely inspired by AppNexus' Bonsai language.

The language has a few design goals:
- It should be simple and intuitive to use for people with some exposure to
  Python,
- It should be difficult to write programs that fail.

The language itself is statically typed, and the type checker allows for the
types of the context macros (the program elements with prefix `$`) to be
provided by an external provider.

The repository provides an interpreter that executes programs inside a
monad. This allows for the values of context macros to be externally provided.

## Example

This example does not run it. It is an indication of where to project is going.

```
placholder title: text
placeholder pancake: image

if $creative.size == (640, 480):
    if $geo.city == "Amsterdam":
        if $weather.temperature > 20:
            pancake = "amsterdam-pancake.png"
            title = "Summer in Amsterdam!"
        end
    elif $geo.city == "Paris":
        pancake = "paris-pancake.png"
    else:
        pancake = "small-pancake.png"
    end
end

if $time.part == "evening":
    title = "Good evening"
else:
    title = "Good day!"
end
```

## Project

Administration for the project is kept in a separate org-mode document:
[project.org](docs/project.org).

## References

1. Benjamin C. Pierce. *Types and programming languages*. The MIT press. 2002.
2. Stephen Diehl. *Implementing a JIT compiled language with Haskell and
   LLVM*. 2017. http://www.stephendiehl.com/llvm/
