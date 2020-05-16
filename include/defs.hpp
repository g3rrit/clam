#ifndef DEF_HPP
#define DEF_HPP

#include "std.hpp"
#include <exception>
#include <stdexcept>

struct Config {
    string version;
    string cc;
    bool   verbose;

    string output;
};

extern Config CONFIG;

extern std::exception_ptr _EPTR;

auto static inline CHECKE() {
    if (_EPTR != nullptr) {
        std::rethrow_exception(_EPTR);
    }
}

#define TRY_CATCH(x) \
    do { try { x; } catch (...) { \
        if (_EPTR == nullptr) { _EPTR = std::current_exception(); }  \
    } } while(0)

auto pipe(const Array<File>& files) -> void;

#endif