add_library(lexer lexer.cpp)
target_link_libraries(lexer PUBLIC spdlog::spdlog)
target_compile_features(lexer PUBLIC cxx_std_17)
add_library(jimple_declaration jimple_declaration.cpp)