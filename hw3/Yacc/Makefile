interpreter = gpp_interpreter

gpp_c_file = gpp.c

yacc_file = gpp_interpreter.y

yacc_c_file = parser.tab.c

lexer = gpp_lexer.l

lexer_c_file = gpp_lexer.c

compile: 
	yacc -o $(yacc_c_file) -d $(yacc_file) 
	flex -o $(lexer_c_file) $(lexer)
	gcc $(gpp_c_file) $(lexer_c_file) $(yacc_c_file) -o $(interpreter)

run1: 
	@./$(interpreter)

run2:
	@./$(interpreter) input1.gpp

clean:
	rm $(interpreter) $(yacc_c_file) $(lexer_c_file)  parser.tab.h  output.txt









	