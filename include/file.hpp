#ifndef FILE_HPP
#define FILE_HPP

#include "int_string.hpp"

struct File : string {
    auto to_path() const -> string
    {
        return *this;
    }
};

#endif