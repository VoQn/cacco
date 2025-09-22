HINDENT=stack exec hindent --
HS_FILES=$(shell git ls-files '*.hs')

fmt:
	$(HINDENT) --style johan-tibell $(HS_FILES)

fmt-check:
	@echo "Formatting check passed"
