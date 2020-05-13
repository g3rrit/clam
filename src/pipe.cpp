#include "std.hpp"
#include "config.hpp"
#include "ast_types.hpp"

#include "lexer.hpp"
#include "parser.hpp"

auto parse_file(File& file) 
{
	ast::Module module;

	yyscan_t sc;
	int res;

	yylex_init(&sc);
	yyset_in(stdin, sc);
	res = yyparse(sc, module);
	yylex_destroy(sc);

	cout << "Module i: " << module.i << endl;
}

auto pipe(Config& config, Array<File>& files)
{
	File file;
	parse_file(file);
}