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
install: ~/.xmonad ~/.xmobarrc stack

~/.xmonad:
	ln -s ${PWD} ~/.xmonad
~/.xmobarrc:
	ln -s ~/.xmonad/xmobarrc ~/.xmobarrc

.PHONY: git
git: xmonad-git xmonad-contrib-git xmobar-git

xmonad-git:
	git clone "https://github.com/xmonad/xmonad" xmonad-git
xmonad-contrib-git:
	git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
xmobar-git:
	git clone "https://github.com/jaor/xmobar" xmobar-git

.PHONY: stack
stack: .stack-work
.stack-work: | git
	stack install
