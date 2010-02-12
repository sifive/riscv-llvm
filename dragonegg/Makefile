# Specify the gcc executable you will use the dragonegg plugin with here.
GCC?=gcc-4.5

# Specify the copy of LLVM you will build the plugin against by giving its
# llvm-config here.  To use an installed copy of LLVM, specify the installed
# llvm-config (just 'llvm-config' is enough if llvm-config is in your path).
# It is not necessary to install LLVM to build dragonegg against it.  Instead
# you can do an LLVM build and point LLVM_CONFIG to the copy of llvm-config
# that was created during the build.
LLVM_CONFIG?=llvm-config


PLUGIN=dragonegg.so
PLUGIN_OBJECTS=llvm-cache.o llvm-convert.o llvm-backend.o llvm-debug.o \
	       llvm-types.o bits_and_bobs.o llvm-abi-default.o

TARGET_OBJECT=llvm-target.o
TARGET_SOURCE=$(shell $(TARGET_UTIL) -p)/llvm-target.cpp

TARGET_UTIL_OBJECTS=target.o
TARGET_UTIL=./target

ALL_OBJECTS=$(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(TARGET_UTIL_OBJECTS)


GENGTYPE_INPUT=$(PWD)/llvm-cache.c
GENGTYPE_OUTPUT=$(PWD)/gt-llvm-cache.h


GCCSOURCE_DIR=$(shell $(GCC) -print-file-name=plugin)
TARGET_TRIPLE:=$(shell $(GCC) -v 2>&1 | grep "^Target:" | sed -e "s/^Target: *//")

# NOTE: replace with an informative string when doing a release.
REVISION:=$(shell svnversion -n .)

CFLAGS+=-Wall -Werror -fPIC -g -O2
CFLAGS+=-DIN_GCC -DREVISION=\"$(REVISION)\" -DTARGET_NAME=\"$(TARGET_TRIPLE)\"
CPPFLAGS+=-MD -MP
CXXFLAGS+=$(CFLAGS) $(shell $(LLVM_CONFIG) --cppflags)

LDFLAGS+=$(shell $(LLVM_CONFIG) --libs analysis core ipo scalaropts target) \
	 $(shell $(LLVM_CONFIG) --ldflags)

PLUGIN_CFLAGS+=-I$(GCCSOURCE_DIR)/gcc -I$(GCCSOURCE_DIR)/include \
	       -I$(GCCSOURCE_DIR)/libcpp/include -I$(GCCSOURCE_DIR)/libdecnumber \
	       -I$(shell $(TARGET_UTIL) -p) -I$(shell $(TARGET_UTIL) -o)
PLUGIN_CXXFLAGS+=$(PLUGIN_CFLAGS)


default: $(PLUGIN)

$(TARGET_UTIL_OBJECTS): %.o : utils/%.cpp
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $<

$(TARGET_UTIL): $(TARGET_UTIL_OBJECTS)
	$(CXX) -o $@ $^ $(LDFLAGS)

%.o : %.c $(TARGET_UTIL)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(PLUGIN_CFLAGS) $<

%.o : %.cpp $(TARGET_UTIL)
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $(PLUGIN_CXXFLAGS) $<

$(TARGET_OBJECT): $(TARGET_UTIL)
	$(CXX) -o $@ -c $(CPPFLAGS) $(CXXFLAGS) $(PLUGIN_CXXFLAGS) -I. \
	$(TARGET_SOURCE)

$(PLUGIN): $(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(TARGET_UTIL)
	$(CXX) -shared $(PLUGIN_OBJECTS) $(TARGET_OBJECT) -o $@ $(LDFLAGS) \
	$(shell $(LLVM_CONFIG) --libs $(shell $(TARGET_UTIL) -p))


clean::
	rm -f *.o *.d $(PLUGIN) $(TARGET_UTIL)

-include $(ALL_OBJECTS:.o=.d)

# The following target exists for the benefit of the dragonegg maintainers, and
# is not used in a normal build.
gt-llvm-cache.h:
	cd $(HOME)/GCC/objects/gcc && ./build/gengtype \
	  -P $(GENGTYPE_OUTPUT) $(GCCSOURCE_DIR) gtyp-input.list \
	    $(GENGTYPE_INPUT)
	sed -i "s/ggc_cache_tab .*\[\]/ggc_cache_tab gt_ggc_rc__gt_llvm_cache_h[]/" $(GENGTYPE_OUTPUT)
	sed -i "s/ggc_root_tab .*\[\]/ggc_root_tab gt_pch_rc__gt_llvm_cache_h[]/" $(GENGTYPE_OUTPUT)
