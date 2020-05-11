#include "std.hpp"
#include "config.hpp"

extern void pipe(Config&, Array<File>&);

auto main(int argc, char **argv) -> int {
	(void) argc;
	(void) argv;

	cout << "----- CLAM -----" << endl;

	Config config;
	Array<File> files;
	pipe(config, files);

	return 0;
}