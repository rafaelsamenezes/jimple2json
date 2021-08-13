add_library(lexer lexer.cpp)
target_link_libraries(lexer PUBLIC spdlog::spdlog)
target_compile_features(lexer PUBLIC cxx_std_17)
add_library(jimple_declaration jimple_declaration.cpp)

add_executable(lexer_run lexer_run.cpp)
target_link_libraries(lexer_run lexer)