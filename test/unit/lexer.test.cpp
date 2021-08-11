#define CATCH_CONFIG_MAIN // This tells Catch to provide a main() - only do this in one cpp file
#include <catch2/catch.hpp>
#include <jimple-parser/lexer.hpp>

TEST_CASE("Basic Lexing", "[core][lexer]")
{
  SECTION("Bool Constant")
  {
    std::string test("true false");
    std::istringstream in(test);
    Lexer l(in);
    CHECK(l.get_next_token() == Token::BOOL_CONSTANT);
    CHECK(l.get_next_token() == Token::BOOL_CONSTANT);
  }
  SECTION("Integer Constant")
  {
    std::string test("0x123abca 12456789 1234L 01234 879456123 0x123 0X1345");
    std::istringstream in(test);
    Lexer l(in);
    CHECK(l.get_next_token() == Token::INTEGER_CONSTANT);
    CHECK(l.get_next_token() == Token::INTEGER_CONSTANT);
    CHECK(l.get_next_token() == Token::INTEGER_CONSTANT);
    CHECK(l.get_next_token() == Token::INTEGER_CONSTANT);
    CHECK(l.get_next_token() == Token::INTEGER_CONSTANT);
    CHECK(l.get_next_token() == Token::INTEGER_CONSTANT);
    CHECK(l.get_next_token() == Token::INTEGER_CONSTANT);
  }
  SECTION("Complex Symbols Lexing")
  {
    std::string test(":= == != >= <= << >> >>>");
    std::istringstream in(test);
    Lexer l(in);
    CHECK(l.get_next_token() == Token::COLON_EQUALS);
    CHECK(l.get_next_token() == Token::CMPEQ);
    CHECK(l.get_next_token() == Token::CMPNE);
    CHECK(l.get_next_token() == Token::CMPGE);
    CHECK(l.get_next_token() == Token::CMPLE);
    CHECK(l.get_next_token() == Token::SHL);
    CHECK(l.get_next_token() == Token::SHR);
    CHECK(l.get_next_token() == Token::USHR);
  }
  SECTION("Reserved Words Lexing")
  {
    std::string test("abstract final native public protected private "
      "static synchronized transient volatile strictfp enum annotation "
      "class interface void boolean byte short char int long float double "
      "null_type unknown extends implements breakpoint case catch cmp cmpg "
      "cmpl default entermonitor exitmonitor goto if instanceof interfaceinvoke "
      "lengthof lookupswitch neg new newarray newmultiarray nop ret return "
      "specialinvoke staticinvoke dynamicinvoke tableswitch throw throws virtualinvoke "
      "null from to with cls");
    std::istringstream in(test);
    Lexer l(in);
    CHECK(l.get_next_token() == Token::ABSTRACT);
    CHECK(l.get_next_token() == Token::FINAL);
    CHECK(l.get_next_token() == Token::NATIVE);
    CHECK(l.get_next_token() == Token::PUBLIC);
    CHECK(l.get_next_token() == Token::PROTECTED);
    CHECK(l.get_next_token() == Token::PRIVATE);
    CHECK(l.get_next_token() == Token::STATIC);
    CHECK(l.get_next_token() == Token::SYNCHRONIZED);
    CHECK(l.get_next_token() == Token::TRANSIENT);
    CHECK(l.get_next_token() == Token::VOLATILE);
    CHECK(l.get_next_token() == Token::STRICTFP);
    CHECK(l.get_next_token() == Token::ENUM);
    CHECK(l.get_next_token() == Token::ANNOTATION);
    CHECK(l.get_next_token() == Token::CLASS);
    CHECK(l.get_next_token() == Token::INTERFACE);
    CHECK(l.get_next_token() == Token::VOID);
    CHECK(l.get_next_token() == Token::BOOLEAN);
    CHECK(l.get_next_token() == Token::BYTE);
    CHECK(l.get_next_token() == Token::SHORT);
    CHECK(l.get_next_token() == Token::CHAR);
    CHECK(l.get_next_token() == Token::INT);
    CHECK(l.get_next_token() == Token::LONG);
    CHECK(l.get_next_token() == Token::FLOAT);
    CHECK(l.get_next_token() == Token::DOUBLE);
    CHECK(l.get_next_token() == Token::NULL_TYPE);
    CHECK(l.get_next_token() == Token::UNKNOWN);
    CHECK(l.get_next_token() == Token::EXTENDS);
    CHECK(l.get_next_token() == Token::IMPLEMENTS);
    CHECK(l.get_next_token() == Token::BREAKPOINT);
    CHECK(l.get_next_token() == Token::CASE);
    CHECK(l.get_next_token() == Token::CATCH);
    CHECK(l.get_next_token() == Token::CMP);
    CHECK(l.get_next_token() == Token::CMPG);
    CHECK(l.get_next_token() == Token::CMPL);
    CHECK(l.get_next_token() == Token::DEFAULT);
    CHECK(l.get_next_token() == Token::ENTERMONITOR);
    CHECK(l.get_next_token() == Token::EXITMONITOR);
    CHECK(l.get_next_token() == Token::GOTO);
    CHECK(l.get_next_token() == Token::IF);
    CHECK(l.get_next_token() == Token::INSTANCEOF);
    CHECK(l.get_next_token() == Token::INTERFACEINVOKE);
    CHECK(l.get_next_token() == Token::LENGTHOF);
    CHECK(l.get_next_token() == Token::LOOKUPSWITCH);
    CHECK(l.get_next_token() == Token::NEG);
    CHECK(l.get_next_token() == Token::NEW);
    CHECK(l.get_next_token() == Token::NEWARRAY);
    CHECK(l.get_next_token() == Token::NEWMULTIARRAY);
    CHECK(l.get_next_token() == Token::NOP);
    CHECK(l.get_next_token() == Token::RET);
    CHECK(l.get_next_token() == Token::RETURN);
    CHECK(l.get_next_token() == Token::SPECIALINVOKE);
    CHECK(l.get_next_token() == Token::STATICINVOKE);
    CHECK(l.get_next_token() == Token::DYNAMICINVOKE);
    CHECK(l.get_next_token() == Token::TABLESWITCH);
    CHECK(l.get_next_token() == Token::THROW);
    CHECK(l.get_next_token() == Token::THROWS);
    CHECK(l.get_next_token() == Token::VIRTUALINVOKE);
    CHECK(l.get_next_token() == Token::NULL_);
    CHECK(l.get_next_token() == Token::FROM);
    CHECK(l.get_next_token() == Token::TO);
    CHECK(l.get_next_token() == Token::WITH);
    CHECK(l.get_next_token() == Token::CLS);
  }
  SECTION("Unary Symbol Tokens")
  {
    std::string test(",{};[]():.'=&|^%+-*/<>");
    std::istringstream in(test);
    Lexer l(in);
    CHECK(l.get_next_token() == Token::COMMA);
    CHECK(l.get_next_token() == Token::L_BRACE);
    CHECK(l.get_next_token() == Token::R_BRACE);
    CHECK(l.get_next_token() == Token::SEMICOLON);
    CHECK(l.get_next_token() == Token::L_BRACKET);
    CHECK(l.get_next_token() == Token::R_BRACKET);
    CHECK(l.get_next_token() == Token::L_PAREN);
    CHECK(l.get_next_token() == Token::R_PAREN);
    CHECK(l.get_next_token() == Token::COLON);
    CHECK(l.get_next_token() == Token::DOT);
    CHECK(l.get_next_token() == Token::QUOTE);
    CHECK(l.get_next_token() == Token::EQUALS);
    CHECK(l.get_next_token() == Token::AND);
    CHECK(l.get_next_token() == Token::OR);
    CHECK(l.get_next_token() == Token::XOR);
    CHECK(l.get_next_token() == Token::MOD);
    CHECK(l.get_next_token() == Token::PLUS);
    CHECK(l.get_next_token() == Token::MINUS);
    CHECK(l.get_next_token() == Token::MULT);
    CHECK(l.get_next_token() == Token::DIV);
    CHECK(l.get_next_token() == Token::CMPLT);
    CHECK(l.get_next_token() == Token::CMPGT);
  }
}