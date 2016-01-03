.PHONY: all
all: xmonad run

xmonad: xmonad.hs
	ghc -threaded --make xmonad.hs

clean:
	@ rm -f xmonad xmonad.hi xmonad.o

run: xmonad
	xmonad --restart

.PHONY: pack
pack: xmonad.tar.gz
xmonad.tar.gz: xmonad.hs xmobarrc xmonad Makefile
	tar zvcf xmonad.tar.gz -C .. $(foreach file,$^,.xmonad/${file}) .xmobarrc

.PHONY: install
install:
	ln -s ${PWD} ~/.xmonad
	ln -s ~/.xmonad/xmobarrc ~/.xmobarrc

.PHONY: cabal install-xmonad install-xmobar
cabal: install-xmonad install-xmobar
install-xmonad:
	cabal install xmonad
install-xmobar:
	cabal install xmobar --flags='with_xft with_utf8 with_alsa with_datezone with_threaded'
