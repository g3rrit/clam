#ifndef STD_HPP
#define STD_HPP

#include <cstdlib>
#include <iostream>
#include <cinttypes>
#include <vector>
#include <map>
#include <string>

// TYPES

typedef std::uint8_t u8;
typedef std::uint16_t u16;
typedef std::uint32_t u32;
typedef std::uint64_t u64;
typedef float f32;
typedef double f64;

using string = std::string;

#define null nullptr;

template<class T>
using Array = std::vector<T>;
template<class K, class T>
using Map = std::map<K, T>;

// FUNCTIONS

using std::move;

using std::cout;
using std::endl;

using std::malloc;

#endif