.PHONY: all
all: xmonad run

xmonad: xmonad.hs
	cabal exec -- ghc -threaded --make xmonad.hs

.PHONY: clean
clean:
	@ rm -f xmonad xmonad.hi xmonad.o

.PHONY: run
run: xmonad
	xmonad --restart

.PHONY: doc
doc:
	cabal exec -- haddock --source-module "../%F" -h -o doc $(wildcard *.hs)

.PHONY: pack
pack: xmonad.tar.gz
xmonad.tar.gz: xmonad.hs xmobarrc xmonad Makefile
	tar zvcf xmonad.tar.gz -C .. $(foreach file,$^,.xmonad/${file}) .xmobarrc

.PHONY: install
install:
	ln -s ${PWD} ~/.xmonad
	ln -s ~/.xmonad/xmobarrc ~/.xmobarrc

.PHONY: cabal-sandbox install-xmonad install-xmonad-contrib install-xmobar cabal
cabal: cabal-sandbox install-xmonad install-xmonad-contrib install-xmobar
cabal-sandbox: cabal.sandbox.config .cabal-sandbox
install-xmonad:
	cabal --require-sandbox install xmonad
install-xmonad-contrib:
	cabal --require-sandbox install xmonad-contrib --flags='use_xft'
install-xmobar:
	cabal --require-sandbox install xmobar --flags='with_xft with_utf8 with_alsa with_datezone with_threaded'

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init
