.PHONY: run
run:
	stack build && stack exec doemane -- -d ./data/words.txt
