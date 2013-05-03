all : assets/todo.js assets/todo.css Main

Main : Main.hs App.hs
	ghc Main -static -optl-static -optl-pthread -O


assets/todo.js : $(shell find static/js/ -name "*.js") static/soy/todo.soy.js
	closurebuilder.py \
		--root=static/js/ \
		--root=static/soy/ \
		--output_file=$@ \
		--namespace=todo.entry \
		--compiler_flags="--compilation_level=SIMPLE_OPTIMIZATIONS"

assets/todo.css : $(shell find static/css/ -name "*.gss")
	java -jar ~/ref/js/closure-tools/closure-stylesheets.jar \
		$^ \
		--output-file $@

static/soy/%.soy.js : static/soy/%.soy
	java -jar ~/ref/js/closure-tools/SoyToJsSrcCompiler.jar \
		--outputPathFormat "{INPUT_DIRECTORY}/{INPUT_FILE_NAME}.js" \
		--shouldProvideRequireSoyNamespaces \
		--srcs $<

clean :
	rm *.o Main assets/todo.js
