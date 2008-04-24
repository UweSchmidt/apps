HSCOLOUR        = HsColour

prog		= find-grep-sed

all		:
		./Setup.hs configure && \
		./Setup.hs build
		cp dist/build/find-grep-sed/find-grep-sed .

install		: all
		sudo ./Setup.hs install
		$(MAKE) installbin
doc		:
		$(HSCOLOUR) -print-css > $(HSCOLOUR)
		./Setup.hs haddock --hyperlink-source --hscolour-css=$(HSCOLOUR)
		rm -f $(HSCOLOUR)

clean		:
		./Setup.hs clean

installbin	:
		[ ! -f ~/bin/$(prog) ] || mv -f ~/bin/$(prog) ~/bin/$(prog)~
		cp -f $(prog) ~/bin

test		: $(prog)
		$(prog) hunitTest

