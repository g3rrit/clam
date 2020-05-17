#ifndef ERROR_HPP
#define ERROR_HPP

#include <exception>
#include <utility>
#include <cstdint>

#include "int_string.hpp"

struct Location {
    std::pair<u32, u32> p0;
    std::pair<u32, u32> p1;

    Location(std::uint32_t l, std::uint32_t c)
        : p0(std::make_pair(l, c)), 
          p1(std::make_pair(l, c)) {}

    Location(std::pair<std::uint32_t, std::uint32_t> _p0, std::pair<std::uint32_t, std::uint32_t> _p1) 
        : p0(_p0), p1(_p1) {}
};

auto inline operator+(const Location& l, const Location& r) -> Location
{
    return Location { l.p0, r.p1 };
}

struct Error : std::exception {

    enum Type {
        COMPILER,
        PARSER,
    } type;

    File     file;
    string   msg;
    Location loc;

    Error(string _msg, Location _loc)
        : msg(_msg), loc(_loc) {}

    auto to_string() -> string;

    auto friend operator<<(std::ostream&, const Error&) -> std::ostream&;
};

#endif