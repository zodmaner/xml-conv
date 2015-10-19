#lang racket

(require xml)

;;; Reads comments in an XML file
(read-comments #t)

;;; Drops empty attributes
(xexpr-drop-empty-attributes #t)

;;; Reads an XML file and stores it as Racket's document struct
(define (parse-xml-file xml-file)
  (call-with-input-file xml-file
    (λ (in)
      (read-xml in))))

;;; Converts the content of an XML document into an X-expression
(define (document-content->xexpr document-struct)
  (remove "\n\n" (xml->xexpr (document-element document-struct))))

;;; Converts the prolog of an XML document into a list
(define (document-prolog->list document-struct)
  (map (λ (item)
         (cond
          [(p-i? item) (list (p-i-target-name item)
                             (p-i-instruction item))]
          [(comment? item) (list 'comment
                                 (comment-text item))]))
       (prolog-misc (document-prolog document-struct))))

;;; Converts an XML file into a list consists of two elements:
;;; a prolog list and an X-expression of the XML's content
(define (xml->l-p-xexpr xml-file)
  (define document-struct (parse-xml-file xml-file))
  (list
   (document-prolog->list document-struct)
   (document-content->xexpr document-struct)))

;;; (Pretty) writes the list of a prolog and the content's X-expression
;;; representation to a file
(define (write-l-p-xexpr l-p-xexpr file-path)
  (call-with-output-file file-path
    (λ (out)
      (pretty-write l-p-xexpr out))))

;;; Parses a prolog list back into a format that's suitable for
;;; creating a prolog struct
(define (parse-prolog-list prolog-list)
  (map (λ (item)
         (cond
          [(equal? 'comment (first item))
           (make-comment (second item))]
          [else (make-p-i #f #f
                          (first item)
                          (second item))]))
       prolog-list))

;;; Parses a list of a prolog and X-expression back into Racket's
;;; document struct
(define (parse-l-p-xexpr l-p-xexpr-file)
  (call-with-input-file l-p-xexpr-file
    (λ (in)
      (let ([l-p-xexpr (read in)])
        (make-document (make-prolog
                        (parse-prolog-list (first l-p-xexpr)) #f '())
                       (xexpr->xml (second l-p-xexpr))
                       empty)))))

;;; Generates a new XML file from a list of prolog and X-expression
(define (generate-xml l-p-xexpr-file xml-file)
  (call-with-output-file xml-file
    (λ (out)
      (display-xml (parse-l-p-xexpr l-p-xexpr-file) out))))

