add_executable(test_driver driver.cpp)
target_link_libraries(test_driver PRIVATE lexer spdlog::spdlog)

# Adds a new acceptance based test
function (new_acceptance_test FILE)
    add_test(NAME ACCEPT-${FILE} COMMAND test_driver ${CMAKE_CURRENT_SOURCE_DIR}/${FILE})
endfunction()

new_acceptance_test(HelloWorld.jimple)