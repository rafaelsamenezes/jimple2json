#pragma once
#include "token.hpp"
#include <unordered_map>
#include <istream>
#include <optional>

/**
 * @brief This is a Simple Lexer class to generate the token list
 *
 */
class Lexer {
    public:

    /**
     * @brief Construct a new Lexer object
     *
     * @param in input stream containing the code
     */
    explicit Lexer(std::istream &in) : in(in) {
        TOKEN_MAP = {
        { "boolean", Token::BOOLEAN },
        { "abstract", Token::ABSTRACT },
        { "final", Token::FINAL },
        { "native", Token::NATIVE },
        { "public", Token::PUBLIC },
        { "protected", Token::PROTECTED },
        { "private", Token::PRIVATE },
        { "static", Token::STATIC },
        { "synchronized", Token::SYNCHRONIZED },
        { "transient", Token::TRANSIENT },
        { "volatile", Token::VOLATILE },
        { "strictfp", Token::STRICTFP },
        { "enum", Token::ENUM },
        { "annotation", Token::ANNOTATION },
        { "class", Token::CLASS },
        { "interface", Token::INTERFACE },
        { "void", Token::VOID },
        { "byte", Token::BYTE },
        { "short", Token::SHORT },
        { "char", Token::CHAR },
        { "int", Token::INT },
        { "long", Token::LONG },
        { "float", Token::FLOAT },
        { "double", Token::DOUBLE },
        { "null_type", Token::NULL_TYPE },
        { "unknown", Token::UNKNOWN },
        { "extends", Token::EXTENDS },
        { "implements", Token::IMPLEMENTS },
        { "breakpoint", Token::BREAKPOINT },
        { "case", Token::CASE },
        { "catch", Token::CATCH },
        { "cmp", Token::CMP },
        { "cmpg", Token::CMPG },
        { "cmpl", Token::CMPL },
        { "default", Token::DEFAULT },
        { "entermonitor", Token::ENTERMONITOR },
        { "exitmonitor", Token::EXITMONITOR },
        { "goto", Token::GOTO },
        { "if", Token::IF },
        { "instanceof", Token::INSTANCEOF },
        { "interfaceinvoke", Token::INTERFACEINVOKE },
        { "lengthof", Token::LENGTHOF },
        { "lookupswitch", Token::LOOKUPSWITCH },
        { "neg", Token::NEG },
        { "new", Token::NEW },
        { "newarray", Token::NEWARRAY },
        { "newmultiarray", Token::NEWMULTIARRAY },
        { "nop", Token::NOP },
        { "ret", Token::RET },
        { "return", Token::RETURN },
        { "specialinvoke", Token::SPECIALINVOKE },
        { "staticinvoke", Token::STATICINVOKE },
        { "dynamicinvoke", Token::DYNAMICINVOKE },
        { "tableswitch", Token::TABLESWITCH },
        { "throw", Token::THROW },
        { "throws", Token::THROWS },
        { "virtualinvoke", Token::VIRTUALINVOKE },
        { "null", Token::NULL_ },
        { "from", Token::FROM },
        { "to", Token::TO },
        { "with", Token::WITH },
        { "cls", Token::CLS },
        { ",", Token::COMMA },
        { "$", Token::DOLLAR },
        { "{", Token::L_BRACE },
        { "}", Token::R_BRACE },
        { "[", Token::L_BRACKET },
        { "]", Token::R_BRACKET },
        { "(", Token::L_PAREN },
        { ")", Token::R_PAREN },
        { ":", Token::COLON },
        { ".", Token::DOT },
        { "'", Token::QUOTE },
        { ";", Token::SEMICOLON },
        { "=", Token::EQUALS },
        { "&", Token::AND},
        { "|", Token::OR},
        { "^", Token::XOR},
        { "%", Token::MOD},
        { "+", Token::PLUS},
        { "-", Token::MINUS},
        { "*", Token::MULT},
        { "/", Token::DIV},
        { "<", Token::CMPLT},
        { ">", Token::CMPGT},
        { ":=", Token::COLON_EQUALS},
        { "true", Token::BOOL_CONSTANT},
        { "false", Token::BOOL_CONSTANT}
        // etc
    };
    }

    // Return the Last token seen
    Token get_next_token();
    const char get_token_char() const { return LastChar; }
    const std::string get_token_str() const { return TokenStr; }

    /**
     * @brief Checks if char is a breakline
     *
     * @param c char to be checked
     * @return true
     * @return false
     */
    static bool is_not_crlf(const char c) {
        return c != '\r' && c != '\n';
    }

    static bool is_first_id_char(const char c) {
        return isalpha(c) || c == '_' || c == '$';
    }
    static bool is_escape_char(const char c) {
        return c == '\\' ||  c == '\''
            || c == '\"' || !is_not_crlf(c)
            || c == '\t';
            //c ==  '\ ' || || c == '\.' || ;
    }

    static bool is_simple_id_char(const char c) {
        return is_first_id_char(c) || isdigit(c) || c == '-';
    }

    protected:
    std::istream &in; // input stream object



    private:
    std::unordered_map<std::string, Token> TOKEN_MAP; // Optimization
    std::string TokenStr;
    char LastChar = ' '; // This is the latest char seen
    std::optional<Token> check_binary_token(char initial, const std::unordered_map<char, Token> &secondary, Token unary);
    Token return_last_token();

    enum class INTEGER_MODE {
        DECIMAL,
        OCTAL,
        HEXADECIMAL
    };
    bool is_number(char n, INTEGER_MODE m = INTEGER_MODE::DECIMAL) {
        switch (m)
        {
        case INTEGER_MODE::DECIMAL:
           return isdigit(n);

        case INTEGER_MODE::OCTAL:
           return isdigit(n) && n != '8' && n != '9';

        case INTEGER_MODE::HEXADECIMAL:
           return isxdigit(n);
        }
        return false; // unreachable
    }

    std::optional<Token> check_string_constant();
    std::optional<Token> check_at_identifier();
    std::optional<Token> check_default_identifier();
    std::optional<Token> check_identifier();
};