#include <jimple-parser/lexer.hpp>
#include <spdlog/spdlog.h>

#include <iostream>
// TODO: Refactor this
Token Lexer::get_next_token() {
  TokenStr = "";
  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = in.get();

  if (LastChar == EOF) {
    spdlog::debug("Got EOF");
    return Token::EOF_;
  }

  auto T = check_string_constant();
  if(T) return T.value();

  T = check_at_identifier();
  if(T) return T.value();

  T = check_default_identifier();
  if(T) return T.value();

  T = check_identifier();
  if(T) return T.value();


  //

  // (decimal | octal | hex)L? | decimal.decimal
  if (isdigit(LastChar)) {

    TokenStr += LastChar;
    INTEGER_MODE M = INTEGER_MODE::DECIMAL;
    if (LastChar == '0') {
      M = INTEGER_MODE::OCTAL;
      LastChar = in.get();
      TokenStr += LastChar;
      if (LastChar == 'x' || LastChar == 'X') {
        LastChar = in.get();
        M = INTEGER_MODE::HEXADECIMAL;
      }
    } else
      LastChar = in.get();

    while (is_number(LastChar, M)) {
      TokenStr += LastChar;
      LastChar = in.get();
    }

    if (LastChar == 'L') // optional L at the end
      LastChar = in.get();

    spdlog::debug("Got INTEGER_CONSTANT with {0}", TokenStr);
    return Token::INTEGER_CONSTANT;
  }

  spdlog::debug("Trying a basic token");
  auto v = check_binary_token(':', {{'=', Token::COLON_EQUALS}}, Token::COLON);
  if (v)
    return v.value();

  v = check_binary_token('=', {{'=', Token::CMPEQ}}, Token::EQUALS);
  if (v)
    return v.value();

  v = check_binary_token('!', {{'=', Token::CMPNE}}, Token::ERROR);
  if (v)
    return v.value();

  v = check_binary_token('>', {{'=', Token::CMPGE}, {'>', Token::SHR}},
                         Token::CMPGT);
  if (v) {
    if (v.value() == Token::SHR && LastChar == '>') {
      LastChar = in.get();
      return Token::USHR;
    }
    return v.value();
  }

  return return_last_token();
}

Token Lexer::return_last_token() {
  std::string LastCharStr;
  LastCharStr += LastChar;
  spdlog::debug("Trying {0}", LastChar);
  LastChar = in.get();
  auto check_token = TOKEN_MAP.find(LastCharStr);
  if (check_token == TOKEN_MAP.end()) {
    return Token::ERROR;
  }
  return TOKEN_MAP.at(LastCharStr);
}

std::optional<Token>
Lexer::check_binary_token(char initial,
                          const std::unordered_map<char, Token> &secondary,
                          Token unary) {

  if (LastChar == initial) {
    LastChar = in.get();
    auto check_token = secondary.find(LastChar);
    if (check_token == secondary.end()) {
      return std::optional<Token>(unary);
    }
    LastChar = in.get();
    return std::optional<Token>(check_token->second);
  }
  return std::nullopt;
}

std::optional<Token>
Lexer::check_string_constant()
{
  // string_constant  = '"' string_char* '"';
  if (LastChar == '"') {
    do {
      TokenStr += LastChar;
      LastChar = in.get();
    } while (LastChar != '"' && is_not_crlf(LastChar) && LastChar != EOF);

    if (LastChar == EOF || !is_not_crlf(LastChar)) {
      spdlog::debug("Error while parsing string constant {0}, Char: {1}", TokenStr, (short)LastChar);
      return std::optional<Token>(Token::ERROR);
    }

    TokenStr += LastChar;
    LastChar = in.get();
    spdlog::debug("Got STRING_CONSTANT with {0}", TokenStr);
    return std::optional<Token>(Token::STRING_CONSTANT);
  }
  return std::nullopt;
}

std::optional<Token> Lexer::check_at_identifier()
{
  // at_identifier = '@' (('parameter' dec_digit+ ':') | 'this' ':' |
  // 'caughtexception');
  if (LastChar == '@') {
    do {
      TokenStr += LastChar;
      LastChar = in.get();
    } while (isalpha(LastChar) || LastChar == '_');

    if (TokenStr == "@parameter") {
      // ('parameter' dec_digit+ ':')
      spdlog::debug("Got parameter");
      while (is_number(LastChar)) {
        TokenStr += LastChar;
        LastChar = in.get();
      }

      if(LastChar == ':')
      {
        TokenStr += LastChar;
        LastChar = in.get();
        return std::optional<Token>(Token::AT_IDENTIFIER);
      }

      return Token::ERROR;
    }

    // 'caughtexception'
    if (TokenStr == "@caughtexception") {
      spdlog::debug("Got AT_IDENTIFIER with caughtexception");
      return std::optional<Token>(Token::AT_IDENTIFIER);
    }

    //// 'this' ':'
    if (TokenStr == "@this" && LastChar == ':') {
      spdlog::debug("Got AT_IDENTIFIER with this");
      LastChar = in.get();
      return std::optional<Token>(Token::AT_IDENTIFIER);
    }
  }
  return std::nullopt;
}

// TODO: redo this
std::optional<Token> Lexer::check_default_identifier()
{
   // identifier <.*>
  if (LastChar == '<')
  {
    TokenStr += LastChar;
    LastChar = in.get();

    if (TokenStr == "<")
    {
      if(LastChar == '=')
      {
        TokenStr += LastChar;
        LastChar = in.get();
        return std::optional<Token>(Token::CMPLE);
      }
      if(LastChar == '<')
      {
        TokenStr += LastChar;
        LastChar = in.get();
        return std::optional<Token>(Token::SHL);
      }
      return std::optional<Token>(Token::CMPLT);
    };

    return std::optional<Token>(Token::ERROR);
  }
  return std::nullopt;
}

std::optional<Token> Lexer::check_identifier()
{
  if (is_first_id_char(LastChar) || is_escape_char(LastChar) || LastChar == '\'') { // [a-Z][0-Z | _]+
    bool must_be_full = LastChar == '\'';
    do {
      TokenStr += LastChar;
      LastChar = in.get();
    } while (is_simple_id_char(LastChar) || is_escape_char(LastChar));

    auto check_token = TOKEN_MAP.find(TokenStr);
    if (check_token == TOKEN_MAP.end()) {
       //identifier / reserver_word
      //  (first_id_char | escape_char) (simple_id_char | escape_char)*


      // quote?
      if(LastChar == '\'')
      {
        TokenStr += LastChar;
        LastChar = in.get();
        must_be_full == true;
      }

      // TODO full_identifier =
      //    ((first_id_char | escape_char | quote) (simple_id_char | escape_char)* (quote)? '.')+
      //    (first_id_char | escape_char | quote) (simple_id_char | escape_char)* (quote)?;
      if(LastChar == '.')
      {
        do {
          TokenStr += LastChar;
          LastChar = in.get();
          if(LastChar == '.')
          {
            TokenStr += LastChar;
            LastChar = in.get();
          }     // Maybe it is
        } while((is_first_id_char(LastChar) || is_escape_char(LastChar) || LastChar == '\''));

        spdlog::debug("Got FULL IDENTIFIER with {0}", TokenStr);
        return Token::IDENTIFIER;
      }

      spdlog::debug("Got IDENTIFIER with {0}", TokenStr);
      return std::optional<Token>(Token::IDENTIFIER);
    }
    return std::optional<Token>(check_token->second);
  }
  return std::nullopt;
}