HSCOLOUR        = HsColour

prog		= find-grep-sed

all		:
		runhaskell ./Setup.lhs configure --global && \
		runhaskell ./Setup.lhs build
		cp dist/build/find-grep-sed/find-grep-sed .

install		: all
		sudo runhaskell ./Setup.lhs install --global
		$(MAKE) installbin

doc		:
		$(HSCOLOUR) -print-css > $(HSCOLOUR)
		runhaskell ./Setup.lhs haddock --hyperlink-source --hscolour-css=$(HSCOLOUR)
		rm -f $(HSCOLOUR)

clean		:
		runhaskell ./Setup.lhs clean

installbin	:
		[ ! -f ~/bin/$(prog) ] || mv -f ~/bin/$(prog) ~/bin/$(prog)~
		cp -f $(prog) ~/bin

test		: $(prog)
		$(prog) hunitTest

