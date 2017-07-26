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
#  This file is part of Aorai plug-in of Frama-C.                        #
#                                                                        #
#  Copyright (C) 2007-2015                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#    INRIA (Institut National de Recherche en Informatique et en         #
#           Automatique)                                                 #
#    INSA  (Institut National des Sciences Appliquees)                   #
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

m4_define([plugin_file],Makefile.in)

m4_define([TIS_KERNEL_SHARE_ENV],
          [m4_normalize(m4_esyscmd([echo $TIS_KERNEL_SHARE]))])

m4_define([TIS_KERNEL_SHARE],
	  [m4_ifval(TIS_KERNEL_SHARE_ENV,[TIS_KERNEL_SHARE_ENV],
                                     [m4_esyscmd(tis-kernel -print-path)])])

m4_ifndef([TIS_KERNEL_M4_MACROS],
         [m4_include(TIS_KERNEL_SHARE/configure.ac)]
        )

check_plugin(aorai,PLUGIN_RELATIVE_PATH(plugin_file),[support for Aorai plug-in],yes,yes)

plugin_use_external(aorai,ltltoba)

if test "$ENABLE_AORAI" != "no"; then
  # ltl2ba library
  configure_tool([LTLTOBA],[ltl2ba],[ltl2ba not found.],no)
fi

check_plugin_dependencies

write_plugin_config(Makefile)
