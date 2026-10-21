.PHONY: api.watch
api.watch:
	stack build --no-run-tests --fast MarvelChampions \
		--ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
		--interleaved-output \
		--file-watch \
		$(EXTRA_STACK_FLAGS) \
		--exec "bash -c \"pkill marvel-champions; DEVELOPMENT=true stack exec marvel-champions &\"" \

.PHONY: pedantic.watch
pedantic.watch:
	stack build --no-run-tests --fast MarvelChampions \
		--ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
		--interleaved-output \
		--file-watch \
		--pedantic \
		$(EXTRA_STACK_FLAGS) \

## Generate a tags file for source-code navigation
.PHONY: tags
tags:
	stack exec -- fast-tags --qualified -Rv --nomerge .
