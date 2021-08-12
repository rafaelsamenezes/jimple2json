#include <rapidcheck.h>

#include <vector>
#include <algorithm>
#include <jimple-parser/lexer.hpp>

/*
 * integer_constant = (dec_constant | hex_constant | oct_constant) 'L'?;
 * dec_constant = dec_digit+;
 * hex_constant = '0' ('x' | 'X') hex_digit+;
 * oct_constant = '0' oct_digit+;
 * dec_digit = ['0' .. '9'];
 * hex_digit = dec_digit | ['a' .. 'f'] | ['A' .. 'F'];
 * oct_digit = ['0' .. '7'];
 */


bool integer_token_prp()
{
    rc::check("An integer should be parsed as a token",
            [](const int &v) {
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

int main() {
  integer_token_prp();
  return 0;
}