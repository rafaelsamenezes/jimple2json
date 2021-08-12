#define CATCH_CONFIG_MAIN // This tells Catch to provide a main() - only do this
                          // in one cpp file
#include <catch2/catch.hpp>
#include <jimple-parser/parser.hpp>

TEST_CASE("declaration_parsing", "[core][statement][declaration]") {
  SECTION("Basic_Declaration") {
    std::string test("bool teste;");
    std::istringstream in(test);
    jimple_declaration decl(in);
    CHECK(decl.type_name == "bool");
    CHECK(decl.var_name == "teste");
  }
}