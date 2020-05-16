#ifndef ERROR_HPP
#define ERROR_HPP

#include <exception>

#include "int_string.hpp"

struct Error : std::exception {
    string msg;

    Error(string _msg)
        : msg(_msg) {}
};

#endif