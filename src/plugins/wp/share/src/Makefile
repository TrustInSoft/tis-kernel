############################################################################
#                                                                          #
#  This file is part of TrustInSoft Kernel.                                #
#                                                                          #
#  TrustInSoft Kernel is a fork of Frama-C. All the differences are:       #
#    Copyright (C) 2016-2017 TrustInSoft                                   #
#                                                                          #
#  TrustInSoft Kernel is released under GPLv2                              #
#                                                                          #
############################################################################

##########################################################################
#                                                                        #
#  This file is part of WP plug-in of Frama-C.                           #
#                                                                        #
#  Copyright (C) 2007-2015                                               #
#    CEA (Commissariat a l'energie atomique et aux energies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# --------------------------------------------------------------------------
# --- Generation of Coq and Alt-ergo files from Why3 one
# --- Developer only
# --------------------------------------------------------------------------

REALIZATION_DIR=..

.PHONY: all realize headers

all: stamp aux realize

stamp:
	mkdir -p stamp

aux:
	mkdir -p aux

realize_with_headers: 

# performs realization and adds headers
realize: stamp/headers.stamp \
     stamp/realizationtools.stamp stamp/coqrealization.stamp \
     stamp/altergorealization.stamp stamp/why3realization.stamp stamp/coqlib.stamp

# To add headers without doing more...
headers:
	@echo "Apply headers (on all wp/share directory)"
	@$(MAKE) -f Makefile.headers -C .. headers > /dev/null
	@touch stamp/headers.stamp

# Dependency needed to allow make -j
stamp/headers.stamp: stamp/realizationtools.stamp stamp/coqrealization.stamp \
     stamp/altergorealization.stamp stamp/why3realization.stamp stamp/coqlib.stamp
	@echo "Apply headers (on some files of wp/share directory)"
	@$(MAKE) -f Makefile.headers -C .. headers.wp_share_src > /dev/null
	@touch $@


ifndef WHY3_SHARE
WHY3_SHARE:=$(shell why3 --print-datadir)
endif

ifndef WHY3_LIB
WHY3_LIB:=$(shell why3 --print-libdir)/coq
endif

# When installing why3, "make install-lib" is required to be able to find the package
.PHONY: info help
help info:
	@echo "Info about targets:"
	@echo "- realize        -> updates all share/prover directories from source files of share/src"
	@echo "- headers        -> adds headers to all necessary files"
	@echo "- compile        -> compiles Coq files"
	@echo "- File.coqide    -> runs CoqIde on File.v"
	@echo "- install_why3_coq WHY3SRC=...  -> install why3 realization for coq  "
	@echo "                    where WHY3SRC directory is the source directory of why3"
	@echo "- tests          -> runs tests"
	@echo "- clean          -> cleaning the directory"
	@echo "- all (default)  -> idem realize"
	@echo "About why3 configuration:"
	@echo "  WHY3_PACKAGE= $(shell ocamlfind query why3)"
	@echo "  WHY3_SHARE= $(WHY3_SHARE)"
	@echo "  WHY3_LIB= $(WHY3_LIB)"

#-- The little plugin that define the meta and transformation keep_it and the
#-- printer that do the realization for altergo
PLUGINS=filter_axioms alt_ergo_realize why3printer_realize

PLUGINS_FILE= $(addsuffix .cmxs, $(PLUGINS)) $(addsuffix .cmo, $(PLUGINS))

plugins: $(PLUGINS_FILE)
	touch plugins

%.cmxs: %.ml
	ocamlfind ocamlopt -package why3 -shared -o $*.cmxs $*.ml

%.cmo: %.ml
	ocamlfind ocamlc -package why3 -c -o $*.cmo $*.ml

#-- Library realization (Coq and Alt-ergo) --

#Realized libraries
WHY3_FILE=qed cmath cint cbits memory vset cfloat vlist

WHY3_COQ_REALIZE_DRIVER=$(WHY3_SHARE)/drivers/coq-realize.drv

WHY3_ALTERGO_DRIVER=$(WHY3_SHARE)/drivers/alt_ergo


WHY3_REALIZE=why3 realize --extra-config realization.conf

stamp/realizationtools.stamp: plugins coq-realize.drv alt_ergo-realize.drv realization.conf stamp/REALIZATION_DIR.stamp
	@touch $@

stamp/coqrealization.stamp: $(addprefix stamp/, $(addsuffix .coq.stamp, $(WHY3_FILE))) stamp
	@touch $@

#file written directly in coq
WPCOQLIB=Bits.v Zbits.v Qedlib.v

stamp/coqlib.stamp: $(WPCOQLIB) stamp/REALIZATION_DIR.stamp
	@echo "Installing Coq Libraries"
	@$(foreach file, $(WPCOQLIB),\
		install -m 444 $(file) $(REALIZATION_DIR)/coqwp;\
		install -m 444 $(file) $(REALIZATION_DIR)/why3;)
	@touch $@

stamp/REALIZATION_DIR.stamp: stamp
	@echo "Create share sub-directories"
	@mkdir -p $(REALIZATION_DIR)/ergo
	@mkdir -p $(REALIZATION_DIR)/why3
	@mkdir -p $(REALIZATION_DIR)/coqwp
	@touch $@

stamp/%.coq.stamp: %.why %.why_theory Makefile $(WHY3_LIB) stamp/realizationtools.stamp
	@echo "Realizing Coq Library for $*"
	@for theory in $$(cat $*.why_theory); do\
	 $(WHY3_REALIZE) -D coq-realize.drv -T $*.$$theory -o .;\
	 install -m 444 $$theory.v $(REALIZATION_DIR)/coqwp;\
	 install -m 444 $$theory.v $(REALIZATION_DIR)/why3;\
	done;
	@touch $@

coq.drv: $(addprefix aux/, $(addsuffix .coq.aux, $(WHY3_FILE)))
	@echo "Making Coq driver"
	@echo "(* generated automatically at developer compilation time *)" | cat - $^ > $@
	install -m 444 $@ $(REALIZATION_DIR)/why3

coq-realize.drv: $(WHY3_COQ_REALIZE_DRIVER) coq.drv
	@echo "Making Coq realization driver"
	@echo "(* generated automatically at compilation time *)" > $@
	@echo "import \"coq.drv\"" >> $@
	@echo "import \"$(WHY3_COQ_REALIZE_DRIVER)\"" >> $@
	@echo "import \"coq-own-realization.drv\"" >> $@

aux/%.coq.aux: %.why %.why_theory Makefile aux
	@echo "Auxiliary Coq files for $*"
	@rm -f $@.tmp;
	@for theory in $$(cat $*.why_theory); do\
	 echo 'theory $*.'"$$theory"' meta "realized_theory" "$*.'"$$theory"'", "'"$$theory"'" end' >> $@.tmp;\
	 done;
	@mv $@.tmp $@

WHY3_STDLIB_USED= \
	bool.Bool \
	map.Map \
	int.Int int.Abs int.MinMax int.ComputerDivision \
	real.Real real.RealInfix real.Abs real.MinMax real.FromInt real.Square

.PHONY: install_why3_coq

install_why3_coq:
	@echo "Installing Why-3 Coq Libraries"
	@install -m 444 $(WHY3SRC)/lib/coq/BuiltIn.v $(REALIZATION_DIR)/coqwp/BuiltIn.v
	@$(foreach file, $(subst .,/,$(WHY3_STDLIB_USED)), \
	   install -m 444 $(WHY3SRC)/lib/coq/$(file).v $(REALIZATION_DIR)/coqwp/$(file).v;)

stamp/altergorealization.stamp:\
	$(addprefix stamp/, \
	$(addsuffix .altergo.stamp, $(WHY3_FILE)) \
	$(addsuffix .altergo.stdlib.stamp, $(WHY3_STDLIB_USED))) stamp
	@touch $@

stamp/%.altergo.stamp: %.why %.why_theory Makefile $(WHY3_LIB) stamp/realizationtools.stamp
	@echo "Realizing Alt-Ergo Library for $*"
	@$(foreach theory, $(shell cat $*.why_theory),\
	 $(WHY3_REALIZE) -D alt_ergo-realize.drv -T $*.$(theory) -o $(REALIZATION_DIR)/ergo;)
	@touch $@


stamp/%.altergo.stdlib.stamp: Makefile $(WHY3_LIB) stamp/realizationtools.stamp
	@echo "Realizing Alt-Ergo Std-Library for $*"
	@mkdir -p tmp; rm -rf tmp/$*; mkdir tmp/$*
	@$(WHY3_REALIZE) -D alt_ergo-realize.drv -T $* -o tmp/$*
	@mv tmp/$*/$(subst .,,$(suffix $*)).mlw $(REALIZATION_DIR)/ergo/$*.mlw
	@rm -rf tmp/$*
	@touch $@


alt_ergo-realize.drv: $(addprefix aux/, $(addsuffix .altergo.aux, $(WHY3_FILE)) $(addsuffix .altergo.stdlib.aux, $(WHY3_STDLIB_USED)))
	@echo "Generating Alt-Ergo driver"
	@echo "(* generated automatically at compilation time *)" > $@
	@echo 'theory BuiltIn meta "realized_theory" "BuiltIn", "BuiltIn_" end' >> $@
	@echo 'theory BuiltIn meta "realized_theory" "Bool", "Bool_" end' >> $@
	@cat $^ >> $@
	@echo "printer \"alt-ergo-realize\"" >> $@
	@echo "filename \"%t.mlw\"" >> $@
	@echo "transformation \"remove_for_altergo\"" >> $@
	@echo "transformation \"inline_in\"" >> $@
	@echo "transformation \"def_into_axiom\"" >> $@
	@echo >> $@
	@echo "import \"alt_ergo_why3_stdlib.drv\"" >> $@
	@echo >> $@
	@echo "(* From why3 alt_ergo.drv except import *)" >> $@
	@cat $(WHY3_ALTERGO_DRIVER).drv | grep -v -e "^import" >> $@
	@echo "(* From why3 alt_ergo_common.drv except printer *)" >> $@
	@cat $(WHY3_ALTERGO_DRIVER)_common.drv | grep -v -e "^printer" -e "^filename" >> $@

#We put the realized theory meta un BuiltIn so that it always appears
aux/%.altergo.aux: %.why %.why_theory Makefile $(WHY3_LIB) aux
	@echo "Auxiliary Alt-Ergo files for $*"
	@rm -f $@.tmp
	@$(foreach theory, $(shell cat $*.why_theory),\
	 echo 'theory $*.$(theory) meta "realized_theory" "$*.$(theory)", "$(subst .,_,$(theory))_" end' >> $@.tmp;)
	@mv $@.tmp $@

aux/%.altergo.stdlib.aux: Makefile $(WHY3_LIB) aux
	@echo "Auxiliary Alt-Ergo stdlib for $*"
	@echo 'theory $* meta "realized_theory" "$*", "$(subst .,_,$*)_" end' > $@;

%.why_theory: %.why
	@echo "Why Theory $<"
	@grep -e "^theory" $< | sed -e  "s/^theory[ ]*//" > $@

#why3 realization
why3.drv: $(addprefix aux/, $(addsuffix .why3.aux, $(WHY3_FILE)))
	@echo "Generating Why-3 driver"
	@echo "(* generated automatically at compilation time *)" | cat - $^ > $@

aux/%.why3.aux: %.why %.why_theory Makefile aux
	@echo "Auxiliary Why-3 files for $*"
	@rm -f $@.tmp;
	@for theory in $$(cat $*.why_theory); do \
	  NAMEUPPER=`ocaml upper.ml "$*"`; \
	  echo 'theory $*.'"$$theory"' meta "realized_theory" "$*.'"$$theory"'", "'"$$NAMEUPPER"'.'"$$theory"'" end' >> $@.tmp;\
	  done;
	@mv $@.tmp $@

stamp/why3realization.stamp:\
	$(addprefix stamp/, $(addsuffix .why3.stamp, $(WHY3_FILE)))
	@touch $@


stamp/%.why3.stamp: %.why %.why_theory Makefile $(WHY3_LIB) stamp/realizationtools.stamp why3-realize.drv why3.drv
	@echo "Realizing Why-3 Theory $*"
	@$(foreach theory, $(shell cat $*.why_theory),\
	 $(WHY3_REALIZE) -D why3-realize.drv -T $*.$(theory) -o $(REALIZATION_DIR)/why3;\
	 why3 prove --type-only -L $(REALIZATION_DIR)/why3 $(REALIZATION_DIR)/why3/$(theory).why)
	@touch $@


COQLIBS:= Qed Qedlib Bits Zbits Cint Cbits Memory Cmath Cfloat Vlist
COQDEP:= $(addsuffix .coqdep, $(COQLIBS))
COQDEP_STAMPS:= $(addprefix stamp/, $(COQDEP))
COQVO:= $(addsuffix .vo, $(COQLIBS))

COQINCLUDE= -R $(WHY3_LIB) -as Why3

%.vo: %.v
	coqc $(COQINCLUDE) $<

stamp/%.coqdep: %.v stamp
	@coqdep $(COQINCLUDE) $< > $@ 2>/dev/null
	@(cmp $@ $(subst stamp/,,$@) 2>/dev/null) \
	|| (cp $@ $(subst stamp/,,$@)  \
	&& echo "Updating $(subst stamp/,,$@)")

stamp/coqdep.stamp: Makefile $(COQDEP_STAMPS)
	@echo "Coq dependencies updated"
	@touch $@

# a non-empty rule is needed for the first make
%.coqdep: stamp/%.coqdep
	@true

include $(COQDEP)

stamp/compile.stamp: Makefile stamp/coqdep.stamp $(COQVO)
	touch $@

.PHONY: compile
compile: stamp/compile.stamp

.PHONY: clean
clean:
	@echo "Cleaning"
	@rm -f $(COQVO)
	@rm -f filter_axioms.cm* filter_axioms.o
	@rm -f alt_ergo_realize.cm* alt_ergo_realize.o
	@rm -f why3printer_realize3.cm* why3printer_realize.o
	@rm -f *.stamp stamp/*
	@rm -f *.aux aux/*
	@rm -f *.coqdep

.PHONY: %.coqide
%.coqide: %.v
	coqide -R $(WHY3_LIB) -as Why3 $<

# --------------------------------------------------------------------------
# ---  Coq Documentation                                                 ---
# --------------------------------------------------------------------------

COQDOC=../doc
COQHTML=../html
COQSRC= $(addsuffix .v,$(COQLIBS))
COQGLOB= $(addsuffix .glob,$(COQLIBS))



html: compile $(COQDOC)/coq2html
	@mkdir -p $(COQHTML)
	@rm -fr $(COQHTML)/*
	@cp $(COQDOC)/frama-c.png $(COQHTML)/
	@cp $(COQDOC)/coq2html.css $(COQDOC)/coq2html.js $(COQDOC)/index.png $(COQHTML)/
	@cat $(COQDOC)/head.html >> $(COQHTML)/index.html
	@for a in $(COQLIBS) ; \
        do echo "  <li> Module <a href=\"$$a.html\">$$a</a></li>" >> $(COQHTML)/index.html ; \
	done ;
	@cat $(COQDOC)/foot.html >> $(COQHTML)/index.html
	$(COQDOC)/coq2html -o $(COQHTML)/%.html $(COQGLOB) $(COQSRC)
	zip ../wpcoqdoc.zip $(COQHTML)/*

latex: $(COQDOC)/coq2latex
	@mkdir -p latex
	@rm -fr latex/*
	coqdoc --latex -d latex --body-only -l $(COQSRC)

$(COQDOC)/coq2html: $(COQDOC)/coq2html.ml
	ocamlfind ocamlopt -o $@ str.cmxa $<

$(COQDOC)/coq2html.ml: $(COQDOC)/coq2html.mll
	ocamllex  $<

$(COQDOC)/coq2latex: $(COQDOC)/coq2latex.ml
	ocamlfind ocamlopt -o $@ str.cmxa $<

$(COQDOC)/coq2latex.ml: $(COQDOC)/coq2latex.mll
	ocamllex $<

##### Generation of configuration file for why3 #####

local_why3.conf: Makefile
	@echo Generation of a local extra-conf for why3
	@printf "[prover_modifiers]\n" > $@
	@printf "name=\"Coq\"\n" >> $@
	@printf "option=\"-R $(PWD)/WP FramaCwp\"\n" >> $@
	@printf "driver=\"$(PWD)/coq.drv\"\n" >> $@
	@printf "\n"  >> $@
	@printf "[editor_modifiers coqide]\n" >> $@
	@printf "option=\"-R $(PWD)/WP FramaCwp\"\n" >> $@
	@printf "\n"  >> $@
	@printf "[editor_modifiers proofgeneral-coq]\n" >> $@
	@printf "option=\"--eval \\\\\"(setq coq-load-path (cons '(\\\\\\\\\\\\\"$(PWD)\\\\\\\\\\\\\" \\\\\\\\\\\\\"FramaCwp\\\\\\\\\\\\\") coq-load-path))\\\\\"\"\n"  >> $@

##### Test local configuration #####

TEST=import
TEST_TARGET:=$(addprefix tests/, $(addsuffix .run, $(TEST)))

tests: $(TEST_TARGET)

tests/%.run: tests/%.why local_why3.conf Makefile
	why3replayer --extra-config local_why3.conf --extra-config realization.conf $<

tests/%.why3ide: tests/%.why local_why3.conf
	why3ide --extra-config local_why3.conf --extra-config realization.conf $<
