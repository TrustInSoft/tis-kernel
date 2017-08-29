############################################################################
#                                                                          #
#  This file is part of tis-interpreter.                                   #
#                                                                          #
#    Copyright (C) 2017 TrustInSoft                                        #
#                                                                          #
#  tis-interpreter is released under GPLv2                                 #
#                                                                          #
############################################################################

EXEC=tis-interpreter

.PHONY: all
all:
	@echo "Nothing to compile for tis-interpreter."
	@echo "Use \`make install\` to install it."

.PHONY: install
install:
	$(eval TISK_SHARE=$(shell tis-kernel-config -print-share-path))
	@test -n "$(TISK_SHARE)" || \
	  (echo "Error: TIS-Kernel $(TISK_SHARE) share path is empty. Missing installation or wrong path?"; \
	   exit 1)
	$(eval BIN_DIR=$(realpath $(TISK_SHARE)/../../bin))
	@echo "Copying $(EXEC) to $(BIN_DIR)"
	@cp $(EXEC) $(BIN_DIR)/$(EXEC)
	@test ! -e $(EXEC)-gui || cp $(EXEC)-gui $(BIN_DIR)/$(EXEC)-gui
