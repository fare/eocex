(export #t)

(import
  :std/misc/ports
  :std/misc/process
  :std/sugar
  :clan/temporary-files)

(def (assemble prog.o prog.s)
  (run-process/batch ["as" "-o" prog.o prog.s]))

(def (link prog.exe . progs.o)
  (run-process/batch ["ld" "-o" prog.exe . progs.o]))

(def (run-prog prog.exe . args)
  (run-process [prog.exe . args]
               coprocess: read-all-as-u8vector))

(def (assemble-and-run source)
  (call-with-temporary-file
   direction: 'output
   prefix: "as"
   suffix: ".s"
   while-open:
   (lambda (port path) (output-contents source port))
   after-close:
   (lambda (path.s)
     (call-with-temporary-file
      direction: 'output
      prefix: "as"
      suffix: ".o"
      after-close:
      (lambda (path.o)
        (assemble path.o path.s)
        (call-with-temporary-file
         direction: 'output
         prefix: "as"
         suffix: ".exe"
         after-close:
         (lambda (path.exe)
           (link path.exe path.o)
           (run-prog path.exe))))))))
