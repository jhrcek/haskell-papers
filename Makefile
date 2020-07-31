.PHONY: format
format: 
	git ls-files '*.hs' | xargs ormolu --mode inplace
