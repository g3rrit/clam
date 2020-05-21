#ifndef DEF_HPP
#define DEF_HPP

#include "std.hpp"
#include <exception>
#include <stdexcept>


// CONFIG

struct Config {
    String version;
    String cc;
    bool   verbose;

    String output;
};

extern Config CONFIG;

// EXCEPTION

extern std::exception_ptr _EPTR;

static inline void CHECKE() {
    if (_EPTR != nullptr) {
        std::rethrow_exception(_EPTR);
    }
}

#define TRY_CATCH(x) \
    do { try { x; } catch (...) { \
        if (_EPTR == nullptr) { _EPTR = std::current_exception(); }  \
    } } while(0)

// PIPE

void pipe(const Array<File>& files);

#endif