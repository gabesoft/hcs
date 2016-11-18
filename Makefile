
build:
	@stack build

serve: build
	@stack exec hcs-exe -- 4444

.PHONY: build serve
