#include "std.hpp"
#include "defs.hpp"
#include "print.hpp"

void init()
{
    THREAD_POOL.init(CONFIG.thread_count);
}

void cleanup()
{
    THREAD_POOL.cleanup();
}

int main(int argc, char **argv)
{
    vprintln("------ CLAM -----");

    init();

    Array<File> files;

    for (int i = 1; i < argc; i++) {
        files.push_back(File { argv[i] });
    }

    pipe(files);

    cleanup();

    return 0;
}