# Essentials of Compilation Exercises

My attempt at learning-by-doing with
[Jeremy Siek](https://wphomes.soic.indiana.edu/jsiek/)’s
[_Essentials of Compilation_](https://github.com/IUCompilerCourse/Essentials-of-Compilation)
as the [textbook](https://www.dropbox.com/s/ktdw8j0adcc44r0/book.pdf?dl=1),
but using [Gerbil Scheme](https://cons.io) as the host language.

## Installing dependencies

```shell
gxpkg deps
```

## Building and testing

```shell
./build.ss && gxtest ./...
```

## Hacking

```shell
gxi -:te
(import :eocex/ch02)
... hack a lot...
(exit) ;; or Ctrl-D at a new line
```
