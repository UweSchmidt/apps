all	: sdist

sdist	: Parser.info1

clean	:
	rm -f Parser.info1 Parser.info Parser.hs

%.info	: %.y
	happy --info -a $<
	rm -rf $*.hs

%.info1	: %.info
	cat $< | cut -b 1-72 > $@

.PHONY	: all clean sdist 
