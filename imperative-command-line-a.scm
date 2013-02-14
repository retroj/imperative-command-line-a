;; Copyright 2011-2013 John J Foerch. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module imperative-command-line-a
        (make-command
         command-name
         command-args
         command-doc
         command-body
         command-name-string
         make-command-group
         add-command-group
         callinfo-name
         callinfo-args
         callinfo-thunk
         parse
         groups
         help-heading
         help-minimum-intercolumn-space)

(import chicken scheme)

(use srfi-1
     srfi-13
     data-structures
     extras
     (only miscmacros dotimes))

;;;
;;; Language
;;;

(define rest cdr)


;;;
;;; Command
;;;

(define-record command
  name args doc body)

(define (command-name-string command-def)
  (symbol->string (command-name command-def)))


;;;
;;; Command Group
;;;

(define groups (make-parameter '()))

(define-syntax %make-command
  (syntax-rules (#:doc)
    ((%make-command (name . args) #:doc doc . body)
     (make-command 'name 'args doc (lambda args . body)))
    ((%make-command (name . args) . body)
     (make-command 'name 'args #f (lambda args . body)))))

(define-syntax make-command-group
  (syntax-rules ()
    ((make-command-group command ...)
     (list (%make-command . command) ...))))

(define-syntax add-command-group
  (syntax-rules ()
    ((add-command-group title . command-defs)
     (groups
      (append!
       (groups)
       (list
        (cons title (make-command-group . command-defs))))))))

(define (find-command-def name command-group)
  (find (lambda (x) (equal? name (command-name-string x)))
        command-group))


;;;
;;; Call Info
;;;

(define-record callinfo
  name args thunk)

(define %make-callinfo make-callinfo)

(define (make-callinfo def args)
  (let ((name (command-name-string def))
        (body (command-body def)))
    (%make-callinfo name args
                    (lambda () (apply body args)))))


;;;
;;; Parser
;;;

(define (parse input . command-groups)
  (let ((out (map (lambda (x) (list)) command-groups)))
    (define (loop input count)
      (if (null? input)
          (apply values out)
          (let* ((opsym (first input))
                 (input (rest input))
                 (count (- count 1))
                 (op (string-trim opsym #\-))
                 (def #f)
                 (group-index (list-index
                               (lambda (group)
                                 (set! def (find-command-def op group))
                                 def)
                               command-groups)))
            (unless def
              (error (sprintf "unexpected symbol ~S~%" opsym)))
            (let ((narg (length (command-args def))))
              (when (< count narg)
                (error (sprintf "~A requires ~A arguments, but only ~A were given"
                                op narg count)))
              (let ((d (list-tail out group-index)))
                (set-car! d (append! (car d) (list (make-callinfo def (take input narg))))))
              (loop (list-tail input narg) (- count narg))))))
    (loop input (length input))))


;;;
;;; Default Command Group(s)
;;;

(define help-heading (make-parameter #f))

(define help-minimum-intercolumn-space (make-parameter 3))

(add-command-group
 "SPECIAL OPTIONS  (evaluate first one and exit)"
 ((help)
  doc: "displays this help"
  (let ((longest
         (fold max 0
               (map
                (lambda (def)
                  (apply + 2 (string-length (command-name-string def))
                         (* 3 (length (command-args def)))
                         (map (compose string-length symbol->string)
                              (command-args def))))
                (append-map cdr (groups))))))
    (define (help-section option-group)
      (for-each
       (lambda (def)
         (let ((col1 (apply string-append " -" (command-name-string def)
                            (map (lambda (a)
                                   (string-append " <" (symbol->string a) ">"))
                                 (command-args def)))))
           (display col1)
           (when (command-doc def)
             (dotimes (_ (+ (help-minimum-intercolumn-space)
                            (- longest (string-length col1))))
               (display " "))
             (display (command-doc def)))
           (newline)))
       option-group))
    (print (help-heading))
    (for-each
     (lambda (group)
       (let ((title (car group))
             (commands (cdr group)))
         (printf "~%~A~%~%" title)
         (help-section commands)))
     (groups))
    (newline)))

 ((version)
  doc: "prints the version"
  (print (help-heading))))

)
