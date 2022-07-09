cores/sudoku.core: sudoku.lisp
	mkdir -p cores/
	make-sbcl-core '(:cgi :scripting :sudoku)' cores/sudoku.core
clean:
	rm -f cores/sudoku.core
install:
	mkdir -p /usr/local/lib/sbcl-cores/
	cp -v cores/sudoku.core /usr/local/lib/sbcl-cores/
uninstall:
	rm -f /usr/local/lib/sbcl-cores/sudoku.core
