all : Main assets/todo.js

Main : Main.hs App.hs
	ghc Main -static -optl-static -optl-pthread -O


assets/todo.js : $(shell find static/js/ -name "*.js")
	closurebuilder.py \
		--root=static/js/ \
		--output_file=$@ \
		--namespace=todo.entry \
		--compiler_flags="--compilation_level=SIMPLE_OPTIMIZATIONS"
clean :
	rm *.o Main assets/todo.js
