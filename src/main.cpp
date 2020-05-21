#include "std.hpp"
#include "defs.hpp"
#include "print.hpp"

int main(int argc, char **argv)
{
    vprintln("------ CLAM -----");

    Config config;
    Array<File> files;

    for (int i = 1; i < argc; i++) {
        files.push_back(File { argv[i] });
    }

    pipe(files);

    return 0;
}