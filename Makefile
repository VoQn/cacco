FOURMOLU=stack exec fourmolu --
HS_FILES=$(shell git ls-files '*.hs')

fmt:
	$(FOURMOLU) -m inplace $(HS_FILES)

fmt-check:
	$(FOURMOLU) -m check $(HS_FILES)
