(import
  :std/sugar
  :std/test
  ./asx86-64)
(export asx86-64-test)

(def N "\n")
(def sys_write 1) ;; use FFI to get them?

(def asx86-64-test
  (test-suite "asx86-64"
    (test-case "easy"
      (check
       (bytes->string
        (assemble-and-run
         (lambda (port)
           (for-each
             (cut display <> port)
             [".globl _start" N
              ".text" N
              "_start:" N
              "movq $1,%rax" N ;; sys_write
              "movq $1,%rdi" N ;; stdout
              "movq $msg,%rsi" N ;; buffer
              "movq $(msg_end-msg),%rdx" N
              "syscall" N
              "movq $60, %rax" N ;; sys_exit
              "movq $0,%rdi" N ;; success
              "syscall" N
              "ret" N
              ".section .rodata" N
              "msg: .ascii \"FOO!\\n\"" N
              "msg_end:" N]))))
       => "FOO!\n"))))
