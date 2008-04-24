HSCOLOUR        = HsColour

prog		= args-parser

all		:
		./Setup.hs configure && \
		./Setup.hs build
		cp dist/build/args-parser/args-parser ./args-parser

install		: all
		sudo ./Setup.hs install

doc		:
		$(HSCOLOUR) -print-css > $(HSCOLOUR)
		./Setup.hs haddock --hyperlink-source --hscolour-css=$(HSCOLOUR)
		rm -f $(HSCOLOUR)

clean		:
		./Setup.hs clean
