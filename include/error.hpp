#ifndef ERROR_HPP
#define ERROR_HPP

#include <exception>
#include <utility>
#include <cstdint>

#include "string.hpp"

struct Location {
    std::pair<u32, u32> p0;
    std::pair<u32, u32> p1;

    Location(std::uint32_t l, std::uint32_t c)
        : p0(std::make_pair(l, c)), 
          p1(std::make_pair(l, c)) {}

    Location(std::pair<std::uint32_t, std::uint32_t> _p0, std::pair<std::uint32_t, std::uint32_t> _p1) 
        : p0(_p0), p1(_p1) {}
};

Location inline operator+(const Location& l, const Location& r)
{
    return Location { l.p0, r.p1 };
}

struct Error : std::exception {

    enum Type {
        COMPILER,
        PARSER,
    } type;

    File     file;
    Location loc;
    String   msg;

    Error(Type _type, File _file, Location _loc, String _msg)
        : type(_type), file(_file), loc(_loc), msg(_msg) {}

    String to_string();

    friend std::ostream& operator<<(std::ostream&, const Error&);
};

#endif