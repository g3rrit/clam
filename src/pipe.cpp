#include "std.hpp"
#include "config.hpp"
#include "ast_types.hpp"

extern FILE* yyin;
extern int yyparse(ast::Module&);

auto parse_file(File& file) 
{
	ast::Module module;

	yyin = stdin;
	
	do {
		yyparse(module);
	} while(std::feof(yyin));

	cout << "Module i: " << module.i << endl;
}

auto pipe(Config& config, Array<File>& files)
{
	File file;
	parse_file(file);
}