set(GCC ${DRAGONEGG_TARGET_GCC})
set(TARGET_TRIPLE "${TARGET_TRIPLE}-gcc-${GCC_MAJOR}.${GCC_MINOR}")
set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/Output")

get_property(DRAGONEGG_PLUGIN TARGET dragonegg PROPERTY LOCATION)

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/dragonegg-lit.site.cfg.in
  ${CMAKE_CURRENT_BINARY_DIR}/dragonegg-lit.site.cfg
  )

# Exclude a couple of tests below out of check-all.
set(EXCLUDE_FROM_ALL ON)

add_lit_testsuite(check-dragonegg-validator "Running the DragonEgg's validator tests"
  ${CMAKE_CURRENT_SOURCE_DIR}/validator
  --config-prefix=validator-lit
  PARAMS site=${CMAKE_CURRENT_BINARY_DIR}/dragonegg-lit.site.cfg
  DEPENDS dragonegg
  llvm-as
  FileCheck count not
  )

add_lit_testsuite(check-dragonegg-compilator "Running the DragonEgg's compilator tests"
  --config-prefix=compilator-lit
  ${CMAKE_CURRENT_SOURCE_DIR}/compilator
  PARAMS site=${CMAKE_CURRENT_BINARY_DIR}/dragonegg-lit.site.cfg
  DEPENDS dragonegg FileCheck
  )
