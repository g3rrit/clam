#ifndef AST_TYPES_HPP
#define AST_TYPES_HPP

#include "std.hpp"

namespace ast {
    
    struct Module;

    struct Unit {
        Array<Module> modules;
    };

    struct Module {
        int i = 1;
    };

    struct Token {
        enum Type {
            INT_VAL,
            DOUBLE_VAL,
            CHAR_VAL,
            STRING_VAL,

            MODULE,
        } type;

        union {
            int int_val;
            double double_val;
            char char_val;
            char* string_val;
        };
    };
}

#endif