.PHONY: all
all: xmonad run

xmonad: xmonad.hs
	xmonad --recompile

.PHONY: clean
clean:
	rm -f xmonad xmonad.hi xmonad.o

.PHONY: run
run: xmonad
	xmonad --restart

.PHONY: doc
doc:
	haddock --source-module "../%F" -h -o doc $(wildcard *.hs)

.PHONY: pack
pack: xmonad.tar.gz
xmonad.tar.gz: xmonad.hs xmobarrc xmonad Makefile
	tar zvcf xmonad.tar.gz -C .. $(foreach file,$^,.xmonad/${file}) .xmobarrc

.PHONY: install
install: ~/.xmonad ~/.xmobarrc

~/.xmonad:
	ln -s ${PWD} ~/.xmonad
~/.xmobarrc:
	ln -s ~/.xmonad/xmobarrc ~/.xmobarrc

.PHONY: install-xmonad install-xmonad-contrib install-xmobar cabal
cabal: install-xmonad install-xmonad-contrib install-xmobar
install-xmonad:
	cabal install xmonad
install-xmonad-contrib:
	cabal install xmonad-contrib --flags='use_xft'
install-xmobar:
	cabal install xmobar --flags='with_xft with_utf8 with_alsa with_datezone with_threaded'
