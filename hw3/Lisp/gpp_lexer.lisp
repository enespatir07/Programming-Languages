;; Define lists for keywords, operators, spaces, and possible operators.
(setq KeyWordList (list "exit"))
(setq KW (list "KW_EXIT"))
(setq OperatorList (list "+" "-" "/" "*" "(" ")"))
(setq OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP"))

(setq Space (list "\n" "\t" " "))
(setq PossibleOperatorList (list "(" ")"))

;; Initialize global variables
(defvar exitValue 0)
(defvar tokenType (list)) 
(defvar valueList (list)) 
(defvar newLineCount 0)

;; Function to determine the token type from the input line
(defun determineTokenType ()
    (splitLine (read-line)) 
)

;; Function to split a line into words and process each word
(defun splitLine (line)
    (let ((words (list)))
        ;; Check if the line is empty and increment newline count
        (if (= (length line) 0)
            (progn
                (setq newLineCount (+ newLineCount 1))
                (return-from splitLine)))

        ;; Trim leading and trailing whitespace
        (setq line (string-trim '(#\Space #\Tab #\Newline) line))
        (setq newLineCount 0)

        ;; Parse words in the line
        (setq words (parseline line))
        (loop for word in words
            do
            ;; Check for the special case of the "exit" keyword
            (if (string= word "(exit)")
                (progn
                    (setq exitValue 1)
                    (setq tokenType (addToListTail "OP_OP" tokenType))
                    (setq tokenType (addToListTail "KW_EXIT" tokenType))
                    (setq tokenType (addToListTail "OP_CP" tokenType))
                    (return)))

            ;; Process each word
            (splitWord word))))


;; Function to split a string into a list of words
(defun split_string (string &optional separators)
  (let ((start 0) (end 0) (result '()))
    (loop while (and (< start (length string))
                     (setf end (or (position-if (lambda (c) (find c separators)) string :start start) 
                                   (length string))))
          do (progn
               (push (subseq string start end) result)
               (setf start (+ 1 end))))
    (nreverse result)))


;; Function to split a word into subwords
(defun splitWord (word)
(let ((subWord) (j 0))  
(loop for i from 1 to (length word)
			do
    (setq subWord (string-downcase (subseq word j i)))
    (if (isoperator subword)    
         (setq j i) 
    )
    (if (isvalue subword)    
         (setq j i) 
    )  
)
))


;; Function to check if a word is an operator
(defun isOperator (word)
    (setq res (searchList word OperatorList))
        (if (not (equal res nil))
        (progn
            (setq tokenType (addToListTail (nth res OP) tokenType))
        )
    )
)


;; Function to check if a string is a valuef
(defun isValue (string)
  (and (stringp string)
       (let ((index (position #\b string)))
         (and index
              (handler-case
                  (let ((first-part (parse-integer (subseq string 0 index)))
                        (second-part (parse-integer (subseq string (1+ index)))))

                     (setq a-valuef (make_Valuef first-part second-part))
                     (push a-valuef valueList)
                     (setq tokenType (addToListTail "VALUEF" tokenType))    
                    t)  

                (error (condition)
                  nil))))))

;; Function to parse a line into a list of parts
(defun ParseLine (str)
(let ((parts (remove-if #'null (mapcar (lambda (x) (string-trim " " x)) (split_string str " ")))))
    (return-from ParseLine parts)
))

;; Function to search for a word in a list
(defun searchList (word listCheck)
    (let ((i 0))
  (loop for item in listCheck
        do (if (string= word item)
               (return-from searchList i)
               nil) 
               (setf i (+ i 1))
  )
  (return-from searchList nil)
))

;; Function to add an item to the end of a list
(defun addToListTail (item list)
    "Adds to the given list"
    (setq list (append list (list item)))
    list
)