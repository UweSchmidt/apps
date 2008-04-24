HSCOLOUR        = HsColour

prog		= find-grep-sed

all		:
		./Setup.lhs configure && \
		./Setup.lhs build
		cp dist/build/find-grep-sed/find-grep-sed .

install		: all
		sudo ./Setup.lhs install
		$(MAKE) installbin
doc		:
		$(HSCOLOUR) -print-css > $(HSCOLOUR)
		./Setup.lhs haddock --hyperlink-source --hscolour-css=$(HSCOLOUR)
		rm -f $(HSCOLOUR)

clean		:
		./Setup.lhs clean

installbin	:
		[ ! -f ~/bin/$(prog) ] || mv -f ~/bin/$(prog) ~/bin/$(prog)~
		cp -f $(prog) ~/bin

test		: $(prog)
		$(prog) hunitTest

