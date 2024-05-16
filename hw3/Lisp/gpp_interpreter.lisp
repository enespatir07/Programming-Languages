;; Load the first file
(load "gpp_lexer.lisp")
;; Initialize global variable
(defvar valNum 0)
;; Main interpreter function
(defun gppinterpreter ()
    (loop
        (determineTokenType)

        ;; Check for double newline, indicating end of input
        (if (= newLineCount 2)
            (progn
                (setq exitValue 1)))

        ;; Start processing tokens
        (start tokenType)

        ;; Check for exit condition
        (if (equal exitValue 1)
            (progn
                (setq exitValue 0)
                (format t "Exiting~%")
                (return)))

        ;; Reset tokenType and valueList for the next iteration
        (setq tokenType (list))
        (setq valueList (list))
        (setq valNum 0)))

;; Function to start processing tokens
(defun start (tokenType)
    (setf isCheck (check tokenType))

    ;; Check syntax and print result
    (if (equal isCheck nil)
        (progn
            (format t "Syntax: Error~%")
            (setq exitValue 1))
        (progn
            (format t "Syntax: Ok~%")
            (if (typep isCheck 'Valuef)
                (format t "Result: ~ab~a ~%" (Valuef-num ischeck) (Valuef-denom ischeck))))))

;; Function to check and process tokens
(defun check (tokenType)
    (setf checkValue nil)
    (if (equal (length tokenType) 1)
        (progn
            (if (string= (nth 0 tokenType) "VALUEF")
                (progn
                    (setf checkValue (nth valNum valueList))
                    (setq valNum (+ valNum 1))
                )
            )

        )
    )
    (if (equal (length tokenType) 3)
        (progn
            (if (and (string= (nth 0 tokenType) "OP_OP") (string= (nth 2 tokenType) "OP_CP"))
            
                (progn
                    (if (string= (nth 1 tokenType) "KW_EXIT")
                        (progn
                            (setf checkValue "exit")
                            (setq exitValue 1)
                        )
                    )
                )

            )
        )
    )
    (if (equal (length tokenType) 5)
        (progn 
            (if (and (string= (nth 0 tokenType) "OP_OP") (string= (nth 4 tokenType) "OP_CP"))
                (progn
                    (if (string= (nth 1 tokenType) "OP_PLUS")
                        (setf checkValue (opPLUS tokenType))
                    )

                    (if (string= (nth 1 tokenType) "OP_MINUS")
                        (setf checkValue (opMINUS tokenType))
                    )

                    (if (string= (nth 1 tokenType) "OP_MULT")
                        (setf checkValue (opMULT tokenType))
                    )

                    (if (string= (nth 1 tokenType) "OP_DIV")
                        (setf checkValue (opDIV tokenType))
                    )
                )
                    
            )
        )
    )
    checkValue
)

;; Structure to represent a valuef

(defstruct Valuef
  num
  denom 
)

;; Function to create a Valuef structure

(defun make_Valuef (_num  _denom)
  (let ((valf (make-Valuef :num _num :denom _denom)))
  (return-from make_valuef valf)
))



;; Functions for basic arithmetic operations

(defun opPLUS (tokenType)
    (setf a (check (list (nth 2 tokenType))))
    (setf b (check (list (nth 3 tokenType))))
    
    (let ((result_num  (+ (* (Valuef-num a) (Valuef-denom b))(* (Valuef-num b) (Valuef-denom a)))))
    (let ((result_denom  (* (Valuef-denom a) (Valuef-denom b))))
    (let ((res (make_valuef result_num  result_denom)))
    (simplify res)
    (return-from opPLUS res)   
))))



(defun opMINUS (tokenType)
    (setf a (check (list (nth 2 tokenType))))
    (setf b (check (list (nth 3 tokenType))))
    (let ((result_num  (- (* (Valuef-num a) (Valuef-denom b))(* (Valuef-num b) (Valuef-denom a)))))
    (let ((result_denom  (* (Valuef-denom a) (Valuef-denom b))))
    (let ((res (make_valuef result_num  result_denom)))
    (simplify res)
    (return-from opMINUS res)   
))))


(defun opMULT (tokenType)
    (setf a (check (list (nth 2 tokenType))))
    (setf b (check (list (nth 3 tokenType))))
    
    (let ((result_num  (* (Valuef-num a) (Valuef-num b))))
    (let ((result_denom  (* (Valuef-denom a) (Valuef-denom b))))
    (let ((res (make_valuef result_num  result_denom)))
    (simplify res)
    (return-from opMULT res)   
))))


(defun opDIV (tokenType)
    (setf a (check (list (nth 2 tokenType))))
    (setf b (check (list (nth 3 tokenType))))
    (let ((result_num  (* (Valuef-num a) (Valuef-denom b))))
    (let ((result_denom  (* (Valuef-denom a) (Valuef-num b))))
    (let ((res (make_valuef result_num  result_denom)))
    (simplify res)
    (return-from opDIV res)   
))))

;; Function to simplify a rational number

(defun simplify (v)
  (let ((div (gcd (Valuef-num v) (Valuef-denom v))))
    (setf (Valuef-num v) (/ (Valuef-num v) div))
    (setf (Valuef-denom v) (/ (Valuef-denom v) div))))

(gppinterpreter)