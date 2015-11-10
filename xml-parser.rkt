#lang racket

(require xml
         threading)

(provide xml-file->x-expr/prolog-file
         x-expr/prolog-file->xml-file)

;; Reads comments in an XML file
(read-comments #t)

;; Drops empty attributes
(xexpr-drop-empty-attributes #t)

;; Reads an XML file and stores it as Racket's document struct
(define (xml-file->document-struct xml-file)
  (call-with-input-file xml-file
    (λ (in)
      (read-xml in))))

;; Recursively removes newline and other control characters from
;; strings in the x-expr
(define (remove-noise-in-list x-expr)
  (let loop ([new-x-expr empty]
             [x-expr x-expr])
    (if (null? x-expr)
        (reverse new-x-expr)
        (let ([elt (car x-expr)])
          (cond
           [(string? elt)
            (if (not (string=? "" (string-normalize-spaces elt)))
                (loop (cons elt new-x-expr) (cdr x-expr))
                (loop new-x-expr (cdr x-expr)))]
           [(list? elt)
            (loop (cons (remove-noise-in-list elt) new-x-expr)
                  (cdr x-expr))]
           [else
            (loop (cons elt new-x-expr) (cdr x-expr))])))))

;; Converts the content of an XML document into an X-expression
(define (document-content->x-expr document-struct)
  (~> document-struct
      (document-element)
      (xml->xexpr)
      (remove-noise-in-list)))

;; Converts the prolog of an XML document into a list
(define (document-prolog->prolog-list document-struct)
  (~>> document-struct
       (document-prolog)
       (prolog-misc)
       (map (λ (item)
              (cond
               [(p-i? item) (list (p-i-target-name item)
                                  (p-i-instruction item))]
               [(comment? item) (list 'comment
                                      (comment-text item))])))))

;; Converts a document struct into a list consists of two elements:
;; a prolog list and an X-expression of the XML's content
(define (document-struct->x-expr/prolog document-struct)
  (list
   (~> document-struct
       (document-prolog->prolog-list))
   (~> document-struct
       (document-content->x-expr))))

;; (Pretty) writes the list of a prolog and the content's X-expression
;; representation to a file
(define (x-expr/prolog->x-expr/prolog-file x-expr/prolog
                                           x-expr/prolog-file)
  (call-with-output-file x-expr/prolog-file
    (λ (out)
      (pretty-write x-expr/prolog out))))

;; Reads (and parses) an XML file and create a new file containing
;; a prolog and X-expression from it
(define (xml-file->x-expr/prolog-file xml-file x-expr/prolog-file)
  (~> xml-file
      (xml-file->document-struct)
      (document-struct->x-expr/prolog)
      (x-expr/prolog->x-expr/prolog-file x-expr/prolog-file)))

;; Parses a prolog list back into a format that's suitable for
;; creating a prolog struct
(define (parse-prolog-list prolog-list)
  (map (λ (item)
         (cond
          [(equal? 'comment (first item))
           (make-comment (second item))]
          [else (make-p-i #f #f
                          (first item)
                          (second item))]))
       prolog-list))

;; Parses a  file containing a list of a prolog and X-expression back
;; into Racket's document struct
(define (x-expr/prolog-file->document-struct x-expr/prolog-file)
  (call-with-input-file x-expr/prolog-file
    (λ (in)
      (let ([x-expr/prolog (read in)])
        (make-document (make-prolog
                        (~> x-expr/prolog
                            (first)
                            (parse-prolog-list))
                        #f '())
                       (~> x-expr/prolog
                           (second)
                           (xexpr->xml))
                       empty)))))

;; (Pretty) writes a document struct representation of an XML file
;; out to a file
(define (document-struct->xml-file document-struct xml-file)
  (call-with-output-file xml-file
    (λ (out)
      (display-xml document-struct out))))

;; Reads (and parses) a file containing a prolog and X-expression and
;; creates a new XML file from it
(define (x-expr/prolog-file->xml-file x-expr/prolog-file xml-file)
  (~> x-expr/prolog-file
      (x-expr/prolog-file->document-struct)
      (document-struct->xml-file xml-file)))
