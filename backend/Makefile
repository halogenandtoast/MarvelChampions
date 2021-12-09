.PHONY: api.watch
api.watch:
	cd marvel-api && \
	stack build --fast marvel-api \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS) \
	  --exec "bash -c \"pkill marvel-api; DEVELOPMENT=true stack exec marvel-api $(EXTRA_STACK_FLAGS) &\"" \

## Generate a tags file for source-code navigation
.PHONY: tags
tags:
	stack exec -- fast-tags --qualified -Rv --nomerge .

.PHONY: core
core:
	cd marvel-core && \
	stack build --fast marvel-core \
	  --ghc-options "-DDEVELOPMENT $(GHC_OPTIONS)" \
	  --interleaved-output \
	  --file-watch \
		$(EXTRA_STACK_FLAGS)

data:
	cd data && \
	curl https://marvel.com/api/public/cards > cards.json && \
	curl https://marvel.com/api/public/packs/ | jq ".[].code" | xargs sh -c 'for arg do mkdir $arg; curl https://marvel.com/api/public/cards/$arg > $arg/cards.json; done' _
