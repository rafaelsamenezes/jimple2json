set(PROPERTY_TEST_LIB rapidcheck)
# Adds a new prp based test
function (new_property_test TARGET SRC LIBS)
  add_executable(${TARGET} ${SRC})
  target_link_libraries(${TARGET} PRIVATE ${LIBS} ${PROPERTY_TEST_LIB})
  target_compile_features(${TARGET} PRIVATE cxx_std_17)
  add_test(NAME ${TARGET}-PRP COMMAND ${TARGET})
endfunction()

new_property_test("rapid" "rapidcheck.cpp" "")

new_property_test("lexer_rapid" "lexer.cpp" "lexer")