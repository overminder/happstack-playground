all : assets/todo.js assets/todo.css Main

Main : $(shell find app -name "*.hs") $(shell find assets/)
	ghc ./app/Main.hs -i./app --make -o $@ \
		-DASSETS_DIR=$(shell pwd)/assets \
		-DSOYS_DIR=$(shell pwd)/static/soy \
		-threaded -static -optl-static -optl-pthread -O
	strip Main

assets/todo.js : $(shell find static/js/ -name "*.js") static/soy/todo.soy.js
	closurebuilder.py \
		--root=static/js/ \
		--root=static/soy/ \
		--output_file=$@ \
		--namespace=todo.entry
		#--compiler_flags="--warning_level=VERBOSE"
		#--compiler_flags="--compilation_level=SIMPLE_OPTIMIZATIONS"
		#--compiler_flags="--use_types_for_optimization" \

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
	rm app/*.o Main assets/todo.js assets/todo.css static/soy/*.soy.js

clean-bin :
	rm app/*.o app/*.hi Main
