#include "defs.hpp"
#include <fstream>

// CONFIG

Config CONFIG = Config {
    .version = "0.0.1",
    .cc = "gcc",
    .verbose = false,
    .output = "./a.out",
};


// ERROR

std::exception_ptr _EPTR = nullptr;

std::ostream& operator<<(std::ostream& os, const Error& error)
{
    switch(error.type) {
    case Error::COMPILER: {
        os << "COMPILER ERROR: " << error.msg << endl;
        return os;
    }
    case Error::PARSER: {
        os << "Parser Error ";
        break;
    }
    }

    auto& [l0, c0] = error.loc.p0;
    auto& [l1, c1] = error.loc.p1;

    os << "in [" << error.file.to_path() << "] at (" 
       << l0 << ", " << c0 << "):" << endl;

    std::ifstream input(error.file.to_path().c_str(), std::ios::in);
    if (!input.is_open()) {
        // maybe throw error here
        return os;
    }

    std::string s;
    for (u32 i = 0; i < l0; i++) {
        std::getline(input, s);
    }

    assert(l1 >= l0);
    for (u32 i = 0; i < l1 - l0; i++) {
        std::getline(input, s);
        os << l0 + 1 << " | " << s << endl;
    }

    return os;
}