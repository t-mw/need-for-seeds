FLAGS = --include=bindings --emit-lua=build/main.lua

debug: build/main.lua

release: FLAGS += --flag=lax
release: build/main.lua

clean:
	rm -r build

build/main.lua: main.lisp
	mkdir -p build
	ln -sF ../assets build
	ln -sF ../lib build
	urn $(FLAGS) main.lisp
