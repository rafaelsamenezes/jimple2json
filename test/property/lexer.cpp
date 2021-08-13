#include <rapidcheck.h>

#include <algorithm>
#include <jimple-parser/lexer.hpp>
#include <vector>

/*
 * integer_constant = (dec_constant | hex_constant | oct_constant) 'L'?;
 * dec_constant = dec_digit+;
 * hex_constant = '0' ('x' | 'X') hex_digit+;
 * oct_constant = '0' oct_digit+;
 * dec_digit = ['0' .. '9'];
 * hex_digit = dec_digit | ['a' .. 'f'] | ['A' .. 'F'];
 * oct_digit = ['0' .. '7'];
 */
bool integer_constant_prp() {
  return rc::check("An integer should be parsed as a token", [](const int &v) {
    constexpr int N = 1000;
    char buf[N] = {0};
    std::ostringstream text;

    // Parse as a decimal value
    snprintf(buf, N, "%u", v);
    text << buf << " ";
    text << buf << "L ";

    // Create octal value
    snprintf(buf, N, "%o", v);
    text << "0" << buf << " ";
    text << "0" << buf << "L ";

    // Create hexadecimal value
    sprintf(buf, "%x", v);
    text << "0x" << buf << " ";
    text << "0X" << buf << " ";
    text << "0x" << buf << "L ";
    text << "0X" << buf << "L ";

    std::istringstream in(text.str());
    Lexer l(in);

    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
    RC_ASSERT(l.get_next_token() == Token::INTEGER_CONSTANT);
  });
}

// at_identifier = '@' (('parameter' dec_digit+ ':') | 'this' ':' |
bool at_identifier_prp() {
  return rc::check("An at_identifier should be parsed as a token", [](const int n) {
    std::ostringstream text;
    constexpr int N = 1000;
    char buf[N] = {0};

    // Checking parameter
    text << "@parameter";

    // Parse as a decimal value
    snprintf(buf, N, "%u", n);

    // Add error string
    text << buf << " ";

    // Add Ok string
    text << "@parameter";
    text << buf << ":";

    std::istringstream in(text.str());
    Lexer L(in);
    RC_ASSERT(L.get_next_token() == Token::ERROR);
    RC_ASSERT(L.get_next_token() == Token::AT_IDENTIFIER);
  });
}


// string_constant  = '"' string_char* '"';
bool string_constant_prp() {
  return rc::check("A string should be parsed as a token", [](const std::vector<char> &v) {
    std::ostringstream Str;
    for(char x : v)
    {
      RC_PRE(x != '"' && x != '\n' && x != '\r' && x != EOF);
      Str << x;
    }
    std::ostringstream oss;
    // Begins with a "
    oss << "\"";
    oss << Str.str();

    std::string unbalanced(oss.str());

    oss << "\"";
    std::string balanced(oss.str());

    std::istringstream in_error(unbalanced);;
    Lexer l_error(in_error);
    RC_ASSERT(l_error.get_next_token() == Token::ERROR);

    std::istringstream in_ok(balanced);;
    Lexer l_ok(in_ok);
    RC_ASSERT(l_ok.get_next_token() == Token::STRING_CONSTANT);
  });
}


int main() {
  integer_constant_prp();
  string_constant_prp();
  at_identifier_prp();
  return 0;
}