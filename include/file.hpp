#ifndef FILE_HPP
#define FILE_HPP

#include "string.hpp"

struct File : String {
    String to_path() const
    {
        return *this;
    }
};

#endif