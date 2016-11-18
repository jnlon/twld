twld: $(TWLDOUT) ;
	ocamlbuild -use-ocamlfind -pkg unix client.native

clean:
	rm -r _build

run:
	./client.native
