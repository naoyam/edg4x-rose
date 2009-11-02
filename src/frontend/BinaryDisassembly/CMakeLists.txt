
include_directories(${KDE4_INCLUDES} ${KDE4_INCLUDE_DIR} ${QT_INCLUDES} )


########### next target ###############


########### next target ###############

#BEGIN ***************** GENERATING armInstructionEnumPrinter.C *******************
FILE(READ x86InstructionEnum.h contents)

file( REMOVE ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C )
file( WRITE  ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C "\#include \"x86InstructionEnum.h\"\n" )
file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C "\#include <string>\n" )

# Convert file contents into a CMake list (where each element in the list
# is one line of the file)
#
STRING(REGEX REPLACE ";" "\\\\;" contents "${contents}")
STRING(REGEX REPLACE "\n" ";" contents "${contents}")
#STRING(REGEX REPLACE ", *//.*;" ".*,,;" contents "${contents}")

set(found_first FALSE)

file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C "static const char* x86InstructionStrings[] = {\n" )
foreach(v ${contents})
  if(v MATCHES "arm_unknown_instruction.*" )
    set(found_first TRUE)
  endif()

  if( found_first )
    string( REGEX REPLACE ", *//.*" "" v2 ${v}  )
    string( REGEX MATCH "[a-zA-Z0-9_]+" v3 ${v2}  )
    set(v4 "\"${v3}\",")
    string( REGEX REPLACE "_record" "." v4 ${v4}  )
    file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C "${v4}\n" )
  endif()
  if( v MATCHES ".*arm_last_instruction.*" )
    break()
  endif()
endforeach()

file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C "\"\"};\n" )
file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C "std::string toString(X86InstructionKind k) {static const int s = sizeof(x86InstructionStrings) / sizeof(*x86InstructionStrings); if ((int)k < 0 || (int)k >= s) return \"\"; else return x86InstructionStrings[(int)k];}\n" )

#END ***************** GENERATING x86InstructionEnumPrinter.C *******************


#BEGIN ***************** GENERATING armInstructionEnumPrinter.C *******************
FILE(READ armInstructionEnum.h contents)

file( REMOVE ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C )
file( WRITE  ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C "\#include \"armInstructionEnum.h\"\n" )
file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C "\#include <string>\n" )

# Convert file contents into a CMake list (where each element in the list
# is one line of the file)
#
STRING(REGEX REPLACE ";" "\\\\;" contents "${contents}")
STRING(REGEX REPLACE "\n" ";" contents "${contents}")
#STRING(REGEX REPLACE ", *//.*;" ".*,,;" contents "${contents}")

set(found_first FALSE)

file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C "static const char* armInstructionStrings[] = {\n" )
foreach(v ${contents})
  if(v MATCHES "arm_unknown_instruction.*" )
    set(found_first TRUE)
  endif()

  if( found_first )
    string( REGEX REPLACE ", *//.*" "" v2 ${v}  )
    string( REGEX MATCH "[a-zA-Z0-9_]+" v3 ${v2}  )
    set(v4 "\"${v3}\",")
    string( REGEX REPLACE "_record" "." v4 ${v4}  )
    file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C "${v4}\n" )
  endif()
  if( v MATCHES ".*arm_last_instruction.*" )
    break()
  endif()
endforeach()

file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C "\"\"};\n" )
file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C "std::string toString(ArmInstructionKind k) {static const int s = sizeof(armInstructionStrings) / sizeof(*armInstructionStrings); if ((int)k < 0 || (int)k >= s) return \"\"; else return armInstructionStrings[(int)k];}\n" )

#END ***************** GENERATING armInstructionEnumPrinter.C *******************

#***************** GENERATING powerpcInstructionEnumPrinter.C *******************

FILE(READ powerpcInstructionEnum.h contents)

file( REMOVE ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C )
file( WRITE  ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C "\#include \"powerpcInstructionEnum.h\"\n" )
file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C "\#include <string>\n" )

# Convert file contents into a CMake list (where each element in the list
# is one line of the file)
#
STRING(REGEX REPLACE ";" "\\\\;" contents "${contents}")
STRING(REGEX REPLACE "\n" ";" contents "${contents}")
#STRING(REGEX REPLACE ", *//.*;" ".*,,;" contents "${contents}")

set(found_first FALSE)

file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C "static const char* powerpcInstructionStrings[] = {\n" )
foreach(v ${contents})
  if(v MATCHES "powerpc_unknown_instruction.*" )
    set(found_first TRUE)
  endif()

  if( found_first )
    string( REGEX REPLACE ", *//.*" "" v2 ${v}  )
    string( REGEX MATCH "[a-zA-Z0-9_]+" v3 ${v2}  )
    set(v4 "\"${v3}\",")
    string( REGEX REPLACE "_record" "." v4 ${v4}  )
    file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C "${v4}\n" )
  endif()
  if( v MATCHES ".*powerpc_last_instruction.*" )
    break()
  endif()
endforeach()

file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C "\"\"};\n" )
file( APPEND ${ROSE_TOP_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C "std::string toString(PowerpcInstructionKind k) {static const int s = sizeof(powerpcInstructionStrings) / sizeof(*powerpcInstructionStrings); if ((int)k < 0 || (int)k >= s) return \"\"; else return powerpcInstructionStrings[(int)k];}\n" )

#END ***************** GENERATING powerpcInstructionEnumPrinter.C *******************




############### TARGET ############################3



#add_library( binary_frontend ${binaryFrontend_SRC} )
#add_dependencies( binary_frontend ROSETTA )





#set( binaryFrontend_SRC RoseBin_support.cpp 
#     instructionDispatch.cpp 
#     x86InstructionEnum.h x86InstructionProperties.h 
#     x86InstructionProperties.C ${CMAKE_BINARY_DIR}/src/frontend/BinaryDisassembly/x86InstructionEnumPrinter.C 
#     armInstructionEnum.h ${CMAKE_BINARY_DIR}/src/frontend/BinaryDisassembly/armInstructionEnumPrinter.C 
#     powerpcInstructionEnum.h ${CMAKE_BINARY_DIR}/src/frontend/BinaryDisassembly/powerpcInstructionEnumPrinter.C 
#     powerpcInstructionProperties.h powerpcInstructionProperties.C 
#     readTicl.C RoseBin_file.cpp RoseFile.cpp 
#     RoseBin_buildTree.cpp RoseBin_IDAPRO_buildTree.cpp 
#     AST_BIN_Traversal.C
#   )

#kde4_add_library( binaryFrontend  ${binaryFrontend_SRC})

#install(TARGETS binaryFrontend ${INSTALL_TARGETS_DEFAULT_ARGS})


########### install files ###############

install(FILES  $(BINARY_DISASSEMBLY_HEADER_FILES) DESTINATION ${INCLUDE_INSTALL_DIR})



#original Makefile.am contents follow:

#include $(top_srcdir)/config/Makefile.for.ROSE.includes.and.libs
#
#INCLUDES = $(ROSE_INCLUDES)  #-I$(srcdir)/graph #-I$(srcdir)/analyses #-I$(srcdir)/mysql_include
#
## binarylibdir = $(libdir)
#
## DQ (1/25/2008): I think we need to use DEV_LIB here
## binarylib_LTLIBRARIES = libbinaryDatabase.la libbinaryFrontend.la libbinarydisassembly.la
## TPS (03/13/08) added ROSE_USE_BINARY_SQL, so that binary analysis can be compiled without MySQL
## This will cause the dynamic library to be built (as libbinaryDatabase.so)
#noinst_LTLIBRARIES = libbinaryFrontend.la #libbinarydisassembly.la
#if ROSE_USE_BINARY_SQL
#  lib_LTLIBRARIES = libbinaryDatabase.la 
#endif
#
#if ROSE_USE_BINARY_SQL
## Isolated part of library that depends on the mysql database
#libbinaryDatabase_la_DEPENDENCIES =
#libbinaryDatabase_la_SOURCES = RoseBin.cpp  RoseBin_DB.cpp  RoseBin_DB_IDAPRO.cpp 
##libbinaryDatabase_la_LIBADD = $(MYSQL_DATABASE_LIBS) -lmysql
#endif 
#
## Isolated part of library that is independent of the mysql database
#libbinaryFrontend_la_DEPENDENCIES =
#libbinaryFrontend_la_SOURCES = RoseBin_support.cpp \
#     instructionDispatch.cpp \
#     x86InstructionEnum.h x86InstructionProperties.h \
#     x86InstructionProperties.C x86InstructionEnumPrinter.C \
#     armInstructionEnum.h armInstructionEnumPrinter.C \
#     powerpcInstructionEnum.h powerpcInstructionEnumPrinter.C \
#     powerpcInstructionProperties.h powerpcInstructionProperties.C \
#     readTicl.C RoseBin_file.cpp RoseFile.cpp \
#     RoseBin_buildTree.cpp RoseBin_IDAPRO_buildTree.cpp \
#     AST_BIN_Traversal.C
#
#
#BUILT_SOURCES = x86InstructionEnumPrinter.C armInstructionEnumPrinter.C powerpcInstructionEnumPrinter.C
#
#CLEANFILES = $(BUILT_SOURCES)
#
#x86InstructionEnumPrinter.C: $(srcdir)/x86InstructionEnum.h
#	echo '#include "x86InstructionEnum.h"' > x86InstructionEnumPrinter.C
#	echo '#include <string>' >> x86InstructionEnumPrinter.C
#	sed -n '/x86_unknown_instruction/,/x86_last_instruction/p' < $(srcdir)/x86InstructionEnum.h | sed 's/ = 0//' | tr -d , | sed 's,^ *,",' | sed 's@$$@",@' | (echo 'static const char* x86InstructionStrings[] = {'; cat; echo '""};') >> x86InstructionEnumPrinter.C
#	echo 'std::string toString(X86InstructionKind k) {static const int s = sizeof(x86InstructionStrings) / sizeof(*x86InstructionStrings); if ((int)k < 0 || (int)k >= s) return ""; else return x86InstructionStrings[(int)k];}' >> x86InstructionEnumPrinter.C
#
#armInstructionEnumPrinter.C: $(srcdir)/armInstructionEnum.h
#	echo '#include "armInstructionEnum.h"' > armInstructionEnumPrinter.C
#	echo '#include <string>' >> armInstructionEnumPrinter.C
#	sed -n '/arm_unknown_instruction/,/arm_last_instruction/p' < $(srcdir)/armInstructionEnum.h | sed 's/ = 0//' | tr -d , | sed 's,^ *,",' | sed 's@$$@",@' | (echo 'static const char* armInstructionStrings[] = {'; cat; echo '""};') >> armInstructionEnumPrinter.C
#	echo 'std::string toString(ArmInstructionKind k) {static const int s = sizeof(armInstructionStrings) / sizeof(*armInstructionStrings); if ((int)k < 0 || (int)k >= s) return ""; else return armInstructionStrings[(int)k];}' >> armInstructionEnumPrinter.C
#
#powerpcInstructionEnumPrinter.C: $(srcdir)/powerpcInstructionEnum.h
#	echo '#include "powerpcInstructionEnum.h"' > powerpcInstructionEnumPrinter.C
#	echo '#include <string>' >> powerpcInstructionEnumPrinter.C
#	sed -n '/powerpc_unknown_instruction/,/powerpc_last_instruction/p' < $(srcdir)/powerpcInstructionEnum.h | sed 's/ = 0//' | sed 's, *//.*,,' | tr -d , | sed 's,^ *,",' | sed 's/_record/./' | sed 's@$$@",@' | (echo 'static const char* powerpcInstructionStrings[] = {'; cat; echo '""};') >> powerpcInstructionEnumPrinter.C
#	echo 'std::string toString(PowerpcInstructionKind k) {static const int s = sizeof(powerpcInstructionStrings) / sizeof(*powerpcInstructionStrings); if ((int)k < 0 || (int)k >= s) return ""; else return powerpcInstructionStrings[(int)k];}' >> powerpcInstructionEnumPrinter.C
#
## Make sure these SAGE specific header files are placed into a distribution
#if ROSE_USE_BINARY_SQL
#BINARY_DISASSEMBLY_HEADER_FILES = \
#   RoseBin_buildTree.h  RoseBin_DB.h  RoseBin_DB_IDAPRO.h \
#   RoseBin.h  RoseBin_IDAPRO_branchGraph.h  RoseBin_IDAPRO_buildTree.h \
#   RoseBin_IDAPRO_callGraph.h  RoseBin_IDAPRO_exprTree.h  RoseBin_IDAPRO_substTree.h \
#   RoseBin_support.h  MyAstAttribute.h \
#   AST_BIN_Traversal.h \
#   readTicl.h RoseBin_file.h RoseFile.h \
#   RoseBin_abstract.h \
#   x86InstructionEnum.h x86InstructionProperties.h powerpcInstructionProperties.h armInstructionEnum.h powerpcInstructionEnum.h \
#   RoseObj.h
#else
#BINARY_DISASSEMBLY_HEADER_FILES = \
#   RoseBin_IDAPRO_branchGraph.h RoseBin_buildTree.h RoseBin_IDAPRO_buildTree.h \
#   RoseBin_IDAPRO_callGraph.h  RoseBin_IDAPRO_exprTree.h  RoseBin_IDAPRO_substTree.h \
#   RoseBin_support.h  MyAstAttribute.h \
#   AST_BIN_Traversal.h \
#   readTicl.h RoseBin_file.h RoseFile.h \
#   RoseBin_abstract.h \
#   x86InstructionEnum.h x86InstructionProperties.h powerpcInstructionProperties.h armInstructionEnum.h powerpcInstructionEnum.h \
#   readTicl.h RoseBin_file.h RoseFile.h \
#   RoseObj.h
#endif
## DQ (1/19/2008): Removed this file from BINARY_DISASSEMBLY_HEADER_FILES since it is not available
## colorTraversal.h  
#
#include_HEADERS = $(BINARY_DISASSEMBLY_HEADER_FILES)
#
#EXTRA_DIST = make-decision-tree.tcl RoseBin_instruction_table_arm RoseBin_instruction_table_x86 #mysql_include
#
## DQ (9/30/2009): Added automake conditional to handle machines where we test and "tclsh" is not availble.
## Rule for maintainers (requires Tcl interpreter)
#$(srcdir)/instructionDispatch.cpp: make-decision-tree.tcl RoseBin_instruction_table_arm RoseBin_instruction_table_x86
#if ROSE_USE_TCLSH
#	cd $(srcdir) && ../../../config/missing --run tclsh ./make-decision-tree.tcl x86 Arm
#else
#   echo "Warning: tclsh is not available on this machine."
#endif
#
#clean-local: