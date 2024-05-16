(defvar operatorFlag 0) ; Flag to determine current char is an operator
(defvar identifierFlag 0) ; Flag to determine if the char is already in a identifier
(defvar commentFlag 0) ; Flag to determine current char has a sign of comment
(defvar temp "")
(defvar opTemp "")
(defvar tokenList "")

(defun gppinterpreter (filename)
  	(cond
		; Reading from input file
		((hasDot filename)
			(with-open-file (stream filename) 
				(loop :for currChar := (read-char stream nil) :while currChar :collect 
					(process currChar) 
				)	
			)	
		)
		; Reading the input that is entered on console
		(t
			(loop for i from 0 below (length filename)
				for char = (char filename i)
				do
				(process2 char filename )
        	)			
		)
			
	)
				



)

;Function to tokenize the input entered in console
(defun process2 (currChar filename )

(cond
		((string= currChar ";") ; Sign of comment, if it repeats again then it is a comment
			(if(eq commentflag 1)
				(printToken "COMMENT")
			)
		)
)

(if(eq commentflag 0)
		(cond
			((char= currChar #\Space ) ; Tokenization is done call printToken
				(printToken temp)
			)			

			((eq (checkOperator currChar) t)
				(setf opTemp (concatenate 'string opTemp (list currChar)))
				(setf operatorFlag 1)
				(setf identifierFlag 0)
				(printToken opTemp)
			)
			; Tokenization of identifiers
			( (alpha-char-p currChar) 
				(setf temp (concatenate 'string temp (list currChar)))
				(setf identifierFlag 1)
				
				(if(string= temp filename)
					(printToken temp)
				)
			)
		)
	)


)

; Function to Tokenize input file
(defun process (currChar)

	(cond
		; If the current char is a ; then it is a sign of a comment but it must be with double ;
		((string= currChar ";") 
			(setf commentFlag 1 )
		)
			
		((char= currChar #\Newline)
			; This case is where the comment is complete 
			(if(eq commentflag 1)
			(printToken "COMMENT")
			)
			; Tokenization is done so call printToken
			(setf commentFlag 0)
			(printToken temp)
		)
	)

	(if(eq commentflag 0)
		(cond
			; Tokenization is done so call printToken
			((char= currChar #\Space ) 
				(printToken temp)
			)
			; Tokenization is done because an operator can include only 1 char long
			((eq (checkOperator currChar) t)
				(setf opTemp (concatenate 'string opTemp (list currChar)))
				(setf operatorFlag 1)
				(setf identifierFlag 0)
				(printToken opTemp)
			)
			; Tokenization of an identifier
			((alpha-char-p currChar)
				(setf temp (concatenate 'string temp (list currChar)))
				(setf identifierFlag 1)
			)
		)
	)

)

; adds given tokens to tokenList according to flags
(defun printToken (token) 
	; If the token is a an operator then add this to temp and print it
	(if(equal operatorFlag 1)
		(progn
		
			
			(cond
				((string= optemp "+")			
					(setf tokenList (concatenate 'string tokenList "OP_PLUS"))
				)
				((string= optemp "-")				
					(setf tokenList (concatenate 'string tokenList "OP_MINUS"))
				)
				((string= optemp "/")				
					(setf tokenList (concatenate 'string tokenList "OP_DIV"))	
				)
				((string= optemp "*")				
					(setf tokenList (concatenate 'string tokenList "OP_MULT"))
				)
				((string= optemp "(")				
					(setf tokenList (concatenate 'string tokenList "OP_OP"))
				)
				((string= optemp ")")			
					(setf tokenList (concatenate 'string tokenList "OP_CP"))
				)
				
				((string= optemp ",")
					(setf tokenList (concatenate 'string tokenList "OP_COMMA"))
				)
			)
			(setf opTemp "")
			(setf operatorFlag 0)
		)
	)
	
	(cond
			; If the token is a comment or identifier or keyword then add this to temp and print it
			; KEYWORDS
			( (string= token "true")
				(setf tokenList (concatenate 'string tokenList "KW_TRUE"))
			)

			((string= token "false")
				(setf tokenList (concatenate 'string tokenList "KW_FALSE"))
			)

			((string= token "and")
				(setf tokenList (concatenate 'string tokenList "KW_AND"))
			)
		
		
			((string= token "or")
				(setf tokenList (concatenate 'string tokenList "KW_OR"))
			)
			
			((string= token "not")
				(setf tokenList (concatenate 'string tokenList "KW_NOT"))
			)


			((string= token "equal")
				(setf tokenList (concatenate 'string tokenList "KW_EQUAL"))
			)


			((string= token "append")
				(setf tokenList (concatenate 'string tokenList "KW_APPEND"))
			)

			((string= token "nil")
				(setf tokenList (concatenate 'string tokenList "KW_NIL"))
			)

			((string= token "list")
				(setf tokenList (concatenate 'string tokenList "KW_LIST"))
			)

			((string= token "less")
				(setf tokenList (concatenate 'string tokenList "KW_LESS"))
			)

			((string= token "concat")
				(setf tokenList (concatenate 'string tokenList "KW_CONCAT"))
			)

			((string= token "set")
				(setf tokenList (concatenate 'string tokenList "KW_SET"))
			)

			((string= token "def")
				(setf tokenList (concatenate 'string tokenList "KW_DEF"))
			)

			((string= token "for")
				(setf tokenList (concatenate 'string tokenList "KW_FOR"))
			)

			((string= token "while")
				(setf tokenList (concatenate 'string tokenList "KW_WHILE"))
			)

			((string= token "if")
				(setf tokenList (concatenate 'string tokenList "KW_IF"))
			)

			((string= token "load")
				(setf tokenList (concatenate 'string tokenList "KW_LOAD"))
			)

			((string= token "display")
				(setf tokenList (concatenate 'string tokenList "KW_DISPLAY"))
			)

			((string= token "exit")
				(setf tokenList (concatenate 'string tokenList "KW_EXIT"))
			)
			;; COMMENT
			( (string= token "COMMENT")
				(setf tokenList (concatenate 'string tokenList "COMMENT"))
			)

			;;IDENTIFIER
			((equal identifierFlag 1)
				(setf tokenList (concatenate 'string tokenList "IDENTIFIER"))
				(setf identifierFlag 0)
			)
			
	)
	
	
	(if(not(string= tokenlist ""))
		(format t "~a~%" tokenList)
	)
	(setf temp "")
	(setq tokenList "")

)

; T if the string is an operator NIL otherwise
(defun checkOperator (operator)
  (if (string= operator "+")
      t
      (if (string= operator "-")
          t
          (if (string= operator "/")
              t
              (if (string= operator "*")
                  t
                  (if (string= operator "(")
                      t
                      (if (string= operator ")")
                          t
                          (if (string= operator ",")
                              t
                              (if (string= operator ";")
                                  t
                                  nil)))))))))



; T if str has '.' in it NIL otherwise
(defun hasDot (str)
  (if (position #\. str :test #'char=)
      t
      nil
    )
)


; Main function
(defun main ()
  
  (let ((console *args*))
  (cond
  ; If the command doesn't include input file run this
	((eq console nil)
		(let ((input (read-line)))
			(gppinterpreter input)
		)
	)
	; If the command  includes input file run this
	(t
		(gppinterpreter (first console))	
		
	)
	
  )
  ))

(main)




