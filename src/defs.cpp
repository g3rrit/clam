#include "defs.hpp"
#include <fstream>
#include <iomanip>

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
        os << "unable to open file";
        return os;
    }

    std::string s;
    for (u32 i = 1; i < l0; i++) {
        std::getline(input, s);
    }

    assert(l1 >= l0);
    for (u32 i = 0; i <= l1 - l0; i++) {
        std::getline(input, s);
        std::ostream tmpos{ NULL };
        tmpos.copyfmt(os);
        os << std::setfill(' ') << std::setw(6) << l0;
        os.copyfmt(tmpos);
        os << " | " << s << endl;
    }

    return os;
}