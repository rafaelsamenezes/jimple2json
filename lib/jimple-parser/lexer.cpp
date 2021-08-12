#include <jimple-parser/lexer.hpp>
#include <spdlog/spdlog.h>

#include <iostream>
Token Lexer::get_next_token() {
    spdlog::debug("Reading next token...");
    // Skip any whitespace.
    while(isspace(LastChar))
      LastChar = in.get();

    if(LastChar == EOF) return Token::EOF;

    if (isalpha(LastChar)) { // [a-Z][0-Z | _]+
        std::string TokenStr;
        do {
            TokenStr += LastChar;
            LastChar = in.get();
        } while (isalnum(LastChar) || LastChar == '_');

        auto check_token = TOKEN_MAP.find(TokenStr);
        if(check_token == TOKEN_MAP.end()) {
            return Token::IDENTIFIER;
        }
        return check_token->second;
    }

    // (decimal | octal | hex)L? | decimal.decimal
    if (isdigit(LastChar))
    {
        std::string TokenStr;
        TokenStr += LastChar;
        INTEGER_MODE M = INTEGER_MODE::DECIMAL;
        if(LastChar == '0') {
            M = INTEGER_MODE::OCTAL;
            LastChar = in.get();
            TokenStr += LastChar;
            if(LastChar == 'x' || LastChar == 'X') {
                LastChar = in.get();
                M = INTEGER_MODE::HEXADECIMAL;
            }
        } else
            LastChar = in.get();

        while (is_number(LastChar, M)) {
            TokenStr += LastChar;
            LastChar = in.get();
        }

        if(LastChar == 'L') // optional L at the end
            LastChar = in.get();

        return Token::INTEGER_CONSTANT;
    }

    auto v = check_binary_token(':',
        {{'=', Token::COLON_EQUALS}}, Token::COLON);
    if(v) return v.value();

    v = check_binary_token('=',
        {{'=', Token::CMPEQ}}, Token::EQUALS);
    if(v) return v.value();

    v = check_binary_token('!',
        {{'=', Token::CMPNE}}, Token::ERROR);
    if(v) return v.value();

    v = check_binary_token('>',
        {{'=', Token::CMPGE},
         {'>', Token::SHR}}, Token::CMPGT);
    if(v) {
        if(v.value() == Token::SHR && LastChar == '>')
        {
            LastChar = in.get();
            return Token::USHR;
        }
        return v.value();
    }

    v = check_binary_token('<',
        {{'=', Token::CMPLE},
         {'<', Token::SHL}}, Token::CMPLT);
    if(v) return v.value();

    return return_last_token();

}

Token Lexer::return_last_token()
{
    std::string LastCharStr;
    LastCharStr += LastChar;
    LastChar = in.get();
    return TOKEN_MAP[LastCharStr];
}

std::optional<Token> Lexer::check_binary_token(char initial, const std::unordered_map<char, Token> &secondary, Token unary) {
    if(LastChar == initial)
    {
        LastChar = in.get();
        auto check_token = secondary.find(LastChar);
        if(check_token == secondary.end())
        {
            return std::optional<Token>(unary);
        }
        LastChar = in.get();
        return std::optional<Token>(check_token->second);
    }
    return std::nullopt;
}