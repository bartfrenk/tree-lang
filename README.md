# TreeLang

## Description

A (non-Turing complete) language for describing decision trees, with syntax
similar to Python. Loosely inspired by AppNexus' Bonsai language.


## Example

```
if $creative.size == "640x480":
    if $geo.city == "Amsterdam":
        pancake = "amsterdam-pancake.png"
        title = "Amsterdam!"
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
   LLVM*. http://www.stephendiehl.com/llvm/
