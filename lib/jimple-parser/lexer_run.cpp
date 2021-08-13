#include <assert.h>
#include <sstream>
#include <jimple-parser/lexer.hpp>
#include <spdlog/spdlog.h>
int main(int argc, const char *argv[]) {
  spdlog::set_level(spdlog::level::debug); // Set global log level to debug
  assert(argc > 1);
  spdlog::info("Reading token: {0}", argv[1]);

  std::string input(argv[1]);
  std::istringstream fin(input);

  Lexer L(fin);
  spdlog::info("Got token: {0}", L.get_next_token());
  return 0;
}