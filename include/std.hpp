#ifndef STD_HPP
#define STD_HPP

#include <cstdlib>
#include <iostream>
#include <cinttypes>
#include <vector>
#include <map>
#include <memory>
#include <thread>

#include "int_string.hpp"
#include "error.hpp"

// TYPES

typedef std::uint8_t u8;
typedef std::uint16_t u16;
typedef std::uint32_t u32;
typedef std::uint64_t u64;
typedef float f32;
typedef double f64;

template <class T>
using uptr = std::unique_ptr<T>;
template <class T>
using sptr = std::shared_ptr<T>;

using Thread = std::thread;

#define null nullptr

// CONTAINER TYPES

template <class T>
using Array = std::vector<T>;
template <class K, class T>
using Map = std::map<K, T>;

// UTILITY TYPES

using File = string;

// FUNCTIONS

using std::move;

using std::cout;
using std::endl;

using std::malloc;

// MACROS

#endif