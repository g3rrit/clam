#ifndef PRINT_HPP
#define PRINT_HPP

#include <iostream>
#include "defs.hpp"

auto static inline fprint(std::ostream&) { return; }

template <class T, class ... Ts>
void fprint(std::ostream &os, const T& arg, const Ts&... args)
{
	std::cout << arg << " ";
	fprint(os, args...);
}

template <class ... Ts>
void fprintln(std::ostream &os, const Ts&... args) 
{
	fprint(os, args...);
	os << std::endl;
}

template <class ... Ts>
void print(const Ts&... args) 
{
	fprint(std::cout, args...);
}

template <class ... Ts>
void println(const Ts&... args) 
{
	fprintln(std::cout, args...);
}

template <class ... Ts>
void vprint(const Ts&... args)
{
    if (CONFIG.verbose) {
        print(args...);
    }
}

template <class ... Ts>
void vprintln(const Ts&... args)
{
    if (CONFIG.verbose) {
        println(args...);
    }
}

#endif