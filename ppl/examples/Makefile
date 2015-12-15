##
##help
##	this target

help	:
	@grep ^## Makefile | sed 's|##||'

GEN	:= $(wildcard gen/*.pgm gen/*ppm)
JPG	= $(patsubst %.pgm,%.jpg,$(patsubst %.ppm,%.jpg,$(GEN)))
PNG	= $(patsubst %.pgm,%.png,$(patsubst %.ppm,%.png,$(GEN)))

# PPLCOPT	= +RTS -K2000000 -A10000000 -M200000000 -Sstderr -G1 -RTS
PPLCOPT	= +RTS -K2000000 -A10000000 -M200000000 -RTS

PPLCOPT	= +RTS -K20m -RTS
PPLC	:= $(shell which pplc)

##
##all
##	compile ppl-test programs
##	run some of the test programs
##	(disabled: and generate java code)

all	: $(PPLC)
	echo $(PPLC)
	[ -d gen ] || mkdir gen
	$(MAKE) test
	$(MAKE) run
	echo $(MAKE) java not done

%.pgm	: org/%.pgm
	cp $< $@

%.ppm	: org/%.ppm
	cp $< $@

%.ppm	: org/%.jpg
	convert $< $@

%.scan	: %.ppl $(PPLC)
	$(PPLC) --scan $< > $@

%.parse	: %.ppl $(PPLC)
	$(PPLC) --parse $< > $@

%.check	: %.ppl $(PPLC)
	$(PPLC) --check $< > $@

%.gencode	: %.ppl $(PPLC)
	$(PPLC) --gencode $< > $@

%.optcode	: %.ppl $(PPLC)
	$(PPLC) --optcode $< > $@

%.diffcode	: %.gencode %.optcode
	-diff -y -t $*.gencode $*.optcode | cut -b 1-32,64- > $@

%.ass	: %.ppl $(PPLC)
	$(PPLC) --ass $< > $@

c/%.c	: %.ppl $(PPLC)
	$(PPLC) --ccode $< > $@

%.o	: %.c vm.h
	$(CC) $(CCFLAGS) -Wall -Wno-unused -c $< || true

java/ppl/%.java	: %.ppl $(PPLC)
	$(PPLC) --java $< > $@

%.jpg	: %.pgm
	convert $< $@

%.jpg	: %.ppm
	convert $< $@

%.png	: %.pgm
	convert $< $@

%.png	: %.ppm
	convert $< $@

%.wavelet.trc	: %.ppm wavelet.ppl $(PPLC)
	$(PPLC) $(PPLCOPT) --exec wavelet.ppl $(basename $<) 2> $@.tab
	expand $@.tab > $@
	rm $@.tab
	mv $*-*.p?m gen

example.trc	: lena.ppm example.ppl $(PPLC)
	$(PPLC) $(PPLCOPT) --exec example.ppl lena 2> $@.tab
	expand $@.tab > $@
	rm $@.tab
	mv lena-*.p?m gen

%.all	: %.ppm example.ppl $(PPLC)
	$(PPLC) $(PPLCOPT) --exec example.ppl $(basename $<) 2> /dev/null
	mv $(basename $<)-*.p?m gen

ggt.trc		: ggt.ppl $(PPLC)
	$(PPLC) --exec $< 2> $@.tab
	expand $@.tab > $@
	rm $@.tab

tailRecursion.trc	: tailRecursion.ppl $(PPLC)
	$(PPLC) --exec $< 2> $@.tab
	expand $@.tab > $@
	rm $@.tab

gamma.trc	: gamma.ppl $(PPLC)
	$(PPLC) --exec $< 2> $@.tab
	expand $@.tab > $@
	rm $@.tab
	mv gamma-*.p?m gen

TESTPROGS	= example.ppl gamma.ppl \
		  stmt.ppl expr.ppl expr0.ppl branch.ppl jump.ppl \
		  ggt.ppl wavelet.ppl tailRecursion.ppl

TESTSCAN	= $(TESTPROGS:.ppl=.scan)
TESTPARSE	= $(TESTPROGS:.ppl=.parse)
TESTCHECK	= $(TESTPROGS:.ppl=.check)
TESTGENCODE	= $(TESTPROGS:.ppl=.gencode)
TESTOPTCODE	= $(TESTPROGS:.ppl=.optcode)
TESTDIFFCODE	= $(TESTPROGS:.ppl=.diffcode)
TESTASS		= $(TESTPROGS:.ppl=.ass)
TESTJAVA	= $(patsubst %.ppl,java/ppl/%.java,$(TESTPROGS))
TESTC		= $(patsubst %.ppl,c/%.c,$(TESTPROGS))
# TESTO		= $(TESTC:.c=.o)
TESTS		= $(TESTSCAN) \
		  $(TESTPARSE) \
		  $(TESTCHECK) \
		  $(TESTGENCODE) \
		  $(TESTOPTCODE) \
		  $(TESTDIFFCODE) \
		  $(TESTASS) \
		  $(TESTJAVA) \
		  $(TESTC) $(TESTO)

##
##test
##	compile ppl test programs

test	: $(TESTS)

RUNS	= \
	example.trc \
	ggt.trc \
	tailRecursion.trc \
	lena.wavelet.trc \
	gamma.trc \
	angela.all papa.all

##
##run
##	run ppl test programs and generate all pictures

run	: $(RUNS)

angela.all	: angela
lena.run	: lena
papa.run	: papa

##
##exrun
##	run the example.ppl program with trace

exrun	:
	$(PPLC) $(PPLCOPT) --exec example.ppl

##
##wavrun
##	run the example.ppl program with trace

wavrun	: lena.ppm wavelet.ppl $(PPLC)
	$(PPLC) $(PPLCOPT) --exec wavelet.ppl lena
	mv *-wave*.p?m gen

##
##java
##	generate java output from test programs
##	and compile the java sources

java	: $(TESTJAVA)
	( cd java ; javac ppl/*.java )

##
##jpgs
##	make jpgs from all ppm and pgm files

jpgs	: $(JPG)

##
##pngs
##	make pngs from all ppm and pgm files

pngs	: $(PNG)

##
##dist
##	generate all distribution files

dist	: pngs

##
##cleantest
##	remove all test program compile output

cleantest	:
	rm -f $(TESTS) $(wildcard java/ppl/*.class)

##
##cleanrun
##

cleanrun	:
	rm -f $(wildcard gen/*.* *.trc)

##
##clean
##	delete all generated files

clean	:
	$(MAKE) cleantest
	$(MAKE) cleanrun

.PHONY	: test run exrun wavrun dist lena angela papa java jpgs pngs cleantest cleanrun clean
