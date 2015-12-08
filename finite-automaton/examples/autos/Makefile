AUTO	= finite-automaton
ECHO	= env echo
AFLAGS	= -v --dot-dir=dot --tab-dir=tab --img-dir=img --code-dir=code \
          --font-size=12,12 --css-ref=auto-svg.css
AAA	= $(AUTO) $(AFLAGS)

SCNs    = scan1 scan2 scan3
NFAs	= nfa1 nfa2 nfa3 nfa4
DFAs    = dfa1 dfa2
REs	= re2 re3 re4 re5 re6 re7 re8 re9 re10 re11 re12 re13 re14 re15 re19 rescan1 rescan2 rescan3
SCANs	= scan/dfa1-1.scan scan/dfa1-2.scan scan/dfa1-3.scan scan/dfa1-4.scan scan/dfa1-5.scan \
	  scan/dfa2-1.scan scan/dfa2-2.scan \
	  scan/nfa1-1.scan scan/nfa1-2.scan scan/nfa1-3.scan scan/nfa1-4.scan scan/nfa1-5.scan \
	  scan/nfa2-1.scan scan/nfa2-2.scan scan/nfa2-3.scan scan/nfa2-4.scan scan/nfa2-5.scan \
	  scan/rescan3-1.scan scan/rescan3-2.scan \
	  scan/scan3-1.scan

all	: scns res nfas dfas scans

scns	: $(SCNs)
res	: $(REs)
nfas	: $(NFAs)
dfas	: $(DFAs)
scans	: $(SCANs)

$(SCNs) $(NFAs) $(DFAs) $(REs) : Makefile dot tab img code
	$(AAA) --example=$@

scan/dfa1-1.scan : scan
	$(ECHO) -n "i id if if12" \
	| $(AAA) --example=dfa1 --scan-dfa-file=- > $@

scan/dfa1-2.scan : scan
	$(ECHO) -n "1 1.00 .01 10." \
	| $(AAA) --example=dfa1 --scan-dfa-file=- > $@

scan/dfa1-3.scan : scan
	$(ECHO) -n "1..20 127.0.0.1" \
	| $(AAA) --example=dfa1 --scan-dfa-file=- > $@

scan/dfa1-4.scan : scan
	$(ECHO) "--xxx" \
	| $(AAA) --example=dfa1 --scan-dfa-file=- > $@

scan/dfa1-5.scan : scan
	$(ECHO) "--nocomment!" \
	| $(AAA) --example=dfa1 --scan-dfa-file=- > $@

scan/dfa2-1.scan : scan
	$(ECHO) -n "aaaaaaaaaab" \
	| $(AAA) --example=dfa2 --scan-dfa-file=- > $@

scan/dfa2-2.scan : scan
	$(ECHO) -n "aaaaaaaaaaa" \
	| $(AAA) --example=dfa2 --scan-dfa-file=- > $@

# nfa scans

scan/nfa1-1.scan : scan
	$(ECHO) -n "" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa1-2.scan : scan
	$(ECHO) -n "aa" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa1-3.scan : scan
	$(ECHO) -n "aaa" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa1-4.scan : scan
	$(ECHO) -n "aaaaaa" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa1-5.scan : scan
	$(ECHO) -n "a" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa2-1.scan : scan
	$(ECHO) -n "" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa2-2.scan : scan
	$(ECHO) -n "aa" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa2-3.scan : scan
	$(ECHO) -n "aaa" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa2-4.scan : scan
	$(ECHO) -n "aaaaaa" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/nfa2-5.scan : scan
	$(ECHO) -n "a" \
	| $(AAA) --example=nfa1 --scan-nfa-file=- > $@

scan/rescan3-1.scan : scan
	$(ECHO) -n "i if if8 42 42. 42.195 .1 --123" \
	| $(AAA) --example=rescan3 --scan-nfa-file=- > $@

scan/rescan3-2.scan : scan
	$(ECHO) -n "i if if8 42 42. 42.195 .1 --123" \
	| $(AAA) --example=rescan3 --scan-dfamin-file=- > $@

# scanner scans

scan/scan3-1.scan : scan
	$(ECHO) -n "i if if8 42 42. 42.195 .1 --123" \
	| $(AAA) --example=scan3 --scan-dfamin-file=- > $@


dot tab img code scan:
	mkdir $@

clean	: rm -f img/*.svg img/*.png dot/*.dot tab/*.tab code/*.hs scan/*.scan

.PHONY 	: all clean $(NFAs) $(DFAs) $(REs) res nfas dfas scans
