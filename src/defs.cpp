#include "defs.hpp"

Config CONFIG = Config {
    .version = "0.0.1",
    .cc = "gcc",
    .verbose = false,
    .output = "./a.out",
};

std::exception_ptr _EPTR = nullptr;