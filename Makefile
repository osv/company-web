all: test

test:
	 cask exec ecukes -r magnars

.PHONY: test
