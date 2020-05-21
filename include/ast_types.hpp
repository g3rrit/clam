#ifndef AST_TYPES_HPP
#define AST_TYPES_HPP

#include "std.hpp"

namespace ast {
    
    struct Module;

    struct Unit {
        Array<Module> modules;
    };

    struct Module {
        File file;
        int i = 1;

        Module(File _file) 
            : file(_file) {}
    };

    struct Id {
        String   val;
        Location loc;
    };

    struct Token {
        enum Type {
            INT_VAL,
            DOUBLE_VAL,
            CHAR_VAL,
            STRING_VAL,

            MODULE,
            ID,
        } type;

        union {
            int int_val;
            double double_val;
            char char_val;
            char* string_val;
            
            Id* id;
        };
    };
}

#endif