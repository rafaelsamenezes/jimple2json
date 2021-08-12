set(UNIT_TEST_LIB Catch2::Catch2)
# Adds a new Unit based test
function (new_unit_test TARGET SRC LIBS)
  add_executable(${TARGET} ${SRC})
  target_link_libraries(${TARGET} PRIVATE ${LIBS} ${UNIT_TEST_LIB})
  target_compile_features(${TARGET} PRIVATE cxx_std_17)
  catch_discover_tests(${TARGET})
endfunction()

new_unit_test(lexer_test "lexer.test.cpp" "lexer")
new_unit_test(jimple_declaration_test "jimple_declaration.test.cpp" "jimple_declaration")