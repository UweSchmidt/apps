HSCOLOUR        = HsColour

prog		= args-parser

all		:
		./Setup.lhs configure && \
		./Setup.lhs build
		cp dist/build/args-parser/args-parser ./args-parser

install		: all
		sudo ./Setup.lhs install

doc		:
		$(HSCOLOUR) -print-css > $(HSCOLOUR)
		./Setup.lhs haddock --hyperlink-source --hscolour-css=$(HSCOLOUR)
		rm -f $(HSCOLOUR)

clean		:
		./Setup.lhs clean
