#include <jimple-parser/lexer.hpp>
#include <spdlog/spdlog.h>

#include <iostream>
Token Lexer::get_next_token()
{
    TokenStr = "";
    // Skip any whitespace.
    while (isspace(LastChar))
        LastChar = in.get();

    if (LastChar == EOF)
    {
        spdlog::debug("Got EOF");
        return Token::EOF_;
    }

    // TODO: string_constant  = '"' string_char* '"';
    if (LastChar == '"')
    {
        do
        {
            TokenStr += LastChar;
            LastChar = in.get();
        } while (LastChar != '"' && is_not_crlf(LastChar) && LastChar != EOF);

        if(LastChar == EOF || !is_not_crlf(LastChar))
        {
            spdlog::error("Error while parsing string constant");
            return Token::ERROR;
        }

        LastChar = in.get();
        spdlog::debug("Got STRING_CONSTANT with {0}", TokenStr);
        return Token::STRING_CONSTANT;
    }
    // TODO: at_identifier = '@' (('parameter' dec_digit+ ':') | 'this' ':' | 'caughtexception');
    if (LastChar == '@')
    {
        do
        {
            TokenStr += LastChar;
            LastChar = in.get();
        } while (isalpha(LastChar) || LastChar == '_');

        if (TokenStr == "parameter")
        {
            // TODO
            // ('parameter' dec_digit+ ':')
            spdlog::debug("Got parameter");
            return Token::ERROR;
        }

        // TODO
        // 'caughtexception'
        if (TokenStr == "cauchtexception")
        {
            spdlog::debug("Got AT_IDENTIFIER with cauchtexception");
            return Token::AT_IDENTIFIER;
        }

        //// 'this' ':'
        if (TokenStr == "this" && LastChar == ':')
        {
            spdlog::debug("Got AT_IDENTIFIER with this");
            LastChar = in.get();
            return Token::AT_IDENTIFIER;
        }
    }

    if (isalpha(LastChar))
    { // [a-Z][0-Z | _]+

        do
        {
            TokenStr += LastChar;
            LastChar = in.get();
        } while (isalnum(LastChar) || LastChar == '_');

        auto check_token = TOKEN_MAP.find(TokenStr);
        if (check_token == TOKEN_MAP.end())
        {
            spdlog::debug("Got IDENTIFIER with {0}", TokenStr);
            return Token::IDENTIFIER;
        }
        return check_token->second;
    }

    // (decimal | octal | hex)L? | decimal.decimal
    if (isdigit(LastChar))
    {

        TokenStr += LastChar;
        INTEGER_MODE M = INTEGER_MODE::DECIMAL;
        if (LastChar == '0')
        {
            M = INTEGER_MODE::OCTAL;
            LastChar = in.get();
            TokenStr += LastChar;
            if (LastChar == 'x' || LastChar == 'X')
            {
                LastChar = in.get();
                M = INTEGER_MODE::HEXADECIMAL;
            }
        }
        else
            LastChar = in.get();

        while (is_number(LastChar, M))
        {
            TokenStr += LastChar;
            LastChar = in.get();
        }

        if (LastChar == 'L') // optional L at the end
            LastChar = in.get();

        spdlog::debug("Got INTEGER_CONSTANT with {0}", TokenStr);
        return Token::INTEGER_CONSTANT;
    }

    spdlog::debug("Trying a basic token");
    auto v = check_binary_token(':',
                                {{'=', Token::COLON_EQUALS}}, Token::COLON);
    if (v)
        return v.value();

    v = check_binary_token('=',
                           {{'=', Token::CMPEQ}}, Token::EQUALS);
    if (v)
        return v.value();

    v = check_binary_token('!',
                           {{'=', Token::CMPNE}}, Token::ERROR);
    if (v)
        return v.value();

    v = check_binary_token('>',
                           {{'=', Token::CMPGE},
                            {'>', Token::SHR}},
                           Token::CMPGT);
    if (v)
    {
        if (v.value() == Token::SHR && LastChar == '>')
        {
            LastChar = in.get();
            return Token::USHR;
        }
        return v.value();
    }

    v = check_binary_token('<',
                           {{'=', Token::CMPLE},
                            {'<', Token::SHL}},
                           Token::CMPLT);
    if (v)
        return v.value();

    return return_last_token();
}

Token Lexer::return_last_token()
{
    std::string LastCharStr;
    LastCharStr += LastChar;
    spdlog::debug("Trying {0}", LastChar);
    LastChar = in.get();
    auto check_token = TOKEN_MAP.find(LastCharStr);
    if (check_token == TOKEN_MAP.end())
    {
        return Token::ERROR;
    }
    return TOKEN_MAP.at(LastCharStr);
}

std::optional<Token> Lexer::check_binary_token(char initial, const std::unordered_map<char, Token> &secondary, Token unary)
{
    if (LastChar == initial)
    {
        LastChar = in.get();
        auto check_token = secondary.find(LastChar);
        if (check_token == secondary.end())
        {
            return std::optional<Token>(unary);
        }
        LastChar = in.get();
        return std::optional<Token>(check_token->second);
    }
    return std::nullopt;
}