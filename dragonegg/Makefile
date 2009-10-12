GCCSOURCE_DIR=$(HOME)/GCC/src/
GCCOBJECT_DIR=$(HOME)/GCC/objects/
# Point LLVM_CONFIG to the just built llvm-config to use an LLVM build rather
# than the installed version of LLVM.
LLVM_CONFIG=llvm-config

# Replace with an informative string when doing a release.
REVISION:=$(shell svnversion -n .)
TARGET_TRIPLE:=$(shell $(GCCOBJECT_DIR)/gcc/xgcc -v 2>&1 | grep "^Target:" | sed -e "s/^Target: *//")

PLUGIN=dragonegg.so
PLUGIN_C=llvm-cache.c
PLUGIN_CPP=llvm-convert.cpp llvm-backend.cpp llvm-debug.cpp llvm-types.cpp \
	   bits_and_bobs.cpp
PLUGIN_C_OBJECTS=$(PLUGIN_C:.c=.o)
PLUGIN_CPP_OBJECTS=$(PLUGIN_CPP:.cpp=.o)
PLUGIN_OBJECTS=$(PLUGIN_C_OBJECTS) $(PLUGIN_CPP_OBJECTS)

TARGET_CPP=$(shell $(TARGET_UTIL) -p)/llvm-target.cpp
TARGET_OBJECT=llvm-target.o

TARGET_UTIL=./target
TARGET_UTIL_OBJECTS=utils/target.o

GENGTYPE_INPUT=$(PWD)/llvm-cache.c
GENGTYPE_OUTPUT=$(PWD)/gt-llvm-cache.h

CFLAGS+=-Wall -Werror -fPIC -g -O2
CFLAGS+=-DIN_GCC -DREVISION=\"$(REVISION)\" -DTARGET_NAME=\"$(TARGET_TRIPLE)\"
CXXFLAGS+=$(CFLAGS) $(shell $(LLVM_CONFIG) --cppflags)

LDFLAGS+=$(shell $(LLVM_CONFIG) --libs analysis core ipo scalaropts target) \
	 $(shell $(LLVM_CONFIG) --ldflags)

PLUGIN_CFLAGS+=-I$(GCCOBJECT_DIR)/gcc -I$(GCCOBJECT_DIR)/gcc/include \
	       -I$(GCCSOURCE_DIR)/gcc -I$(GCCSOURCE_DIR)/include \
	       -I$(GCCSOURCE_DIR)/libcpp/include -I$(GCCSOURCE_DIR)/libdecnumber \
	       -I$(GCCOBJECT_DIR)/libdecnumber -I$(shell $(TARGET_UTIL) -p) \
	       -I$(shell $(TARGET_UTIL) -o)
PLUGIN_CXXFLAGS+=$(PLUGIN_CFLAGS)

default: $(PLUGIN)

$(TARGET_UTIL): $(TARGET_UTIL_OBJECTS)
	$(CXX) $^ -o $@ $(CXXFLAGS) $(LDFLAGS)

$(PLUGIN_C_OBJECTS): %.o : %.c $(TARGET_UTIL)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(PLUGIN_CFLAGS) $<

$(PLUGIN_CPP_OBJECTS): %.o : %.cpp $(TARGET_UTIL)
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $(PLUGIN_CXXFLAGS) $<

$(TARGET_OBJECT): $(TARGET_UTIL)
	$(CXX) -c $(TARGET_CPP) -o $@ $(CPPFLAGS) $(CXXFLAGS) $(PLUGIN_CXXFLAGS) -I.

$(PLUGIN): $(TARGET_UTIL) $(PLUGIN_OBJECTS) $(TARGET_OBJECT)
	$(CXX) -shared $(PLUGIN_OBJECTS) $(TARGET_OBJECT) -o $@ $(LDFLAGS) \
	$(shell $(LLVM_CONFIG) --libs $(shell $(TARGET_UTIL) -p))

llvm-cache.o: gt-llvm-cache.h

gt-llvm-cache.h:
	cd $(GCCOBJECT_DIR)/gcc && ./build/gengtype \
	  -P $(GENGTYPE_OUTPUT) $(GCCSOURCE_DIR) gtyp-input.list \
	    $(GENGTYPE_INPUT)
	sed -i "s/ggc_cache_tab .*\[\]/ggc_cache_tab gt_ggc_rc__gt_llvm_cache_h[]/" $(GENGTYPE_OUTPUT)
	sed -i "s/ggc_root_tab .*\[\]/ggc_root_tab gt_pch_rc__gt_llvm_cache_h[]/" $(GENGTYPE_OUTPUT)

clean::
	rm -f *.o */*.o $(PLUGIN) $(TARGET_UTIL)
