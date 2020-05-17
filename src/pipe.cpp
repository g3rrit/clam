#include "std.hpp"
#include "defs.hpp"
#include "ast_types.hpp"
#include "print.hpp"

extern int parse_file(const File&, ast::Module&);

auto parse(const Array<File>& files, ast::Unit& unit) -> void
{
    Array<Thread> threads;
    u32 i = 0;

    for (const auto& file : files) {

        vprintln("Parsing: ", file);
        unit.modules.push_back(ast::Module {});
        threads.push_back(Thread { [&unit](const File& file, const u32 i) -> void {

            TRY_CATCH(parse_file(file, unit.modules.at(i)));
        }, file, i });
        i++;
    }

    for (auto& thread : threads) {
        thread.join();
    }
    CHECKE();
}

auto pipe(const Array<File>& files) -> void
{
    ast::Unit unit;
    
    try {
        parse(files, unit);
    } catch (Error& error) {
        println(error);
    }

}