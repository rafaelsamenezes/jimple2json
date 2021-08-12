#include <assert.h>
#include <fstream>
#include <jimple-parser/lexer.hpp>
#include <spdlog/spdlog.h>
int main(int argc, const char *argv[]) {
  spdlog::set_level(spdlog::level::debug); // Set global log level to debug
  assert(argc > 1);
  spdlog::info("Reading file: {0}", argv[1]);
  std::ifstream fin(argv[1]);
  assert(fin.is_open());

  Lexer L(fin);
  Token LastToken = Token::ABSTRACT; // Arbitrary value
  while (LastToken != Token::EOF_) {
    LastToken = L.get_next_token();

    assert(LastToken != Token::ERROR);
  }

  return 0;
}