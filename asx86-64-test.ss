(import
  :std/sugar
  :std/test
  ./asx86-64)
(export asx86-64-test)

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
             '(".globl _start \n"
               "_start: \n"
               "movq $1,%rax \n" ;; sys_write
               "movq $1,%rdi \n" ;; stdout
               "movq $msg,%rsi \n" ;; buffer
               "movq $(msg_end-msg),%rdx \n"
               "syscall \n"
               "movq $60, %rax \n" ;; sys_exit
               "movq $0,%rdi \n" ;; success
               "syscall \n"
               "ret \n"
               ".data \n"
               "msg: .ascii \"FOO!\\n\" \n"
               "msg_end: \n")))))
       => "FOO!\n"))))
