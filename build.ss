#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for eocex. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")

;;(displayln "Building eocex")

(import :std/build-script)

(defbuild-script
  `("ex.ss" "ch01.ss"))