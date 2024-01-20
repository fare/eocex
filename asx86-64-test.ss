(import
  :std/sugar
  :std/test
  ./asx86-64)
(export asx86-64-test)

(def sys_write 1) ;; use FFI to get them?
;; https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/


(def asx86-64-test
  (test-suite "asx86-64"
    (test-case "easy"
      (check
       (bytes->string
        (assemble-and-run
         (lambda (port)
           (for-each
             (cut display <> port)
             [(%%start)...
              "movq $1, %rax" N ;; sys_write
              "movq %rax, %rdi" N ;; stdout
              "movq $msg, %rsi" N ;; buffer
              "movq $msg_len, %rdx" N
              "syscall" N
              (%%success-exit)...
              ".section .rodata" N
              "msg: .ascii \"FOO!\\n\"" N
              "msg_len = . - msg" N]))))
       => "FOO!\n"))))
