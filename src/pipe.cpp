#include "std.hpp"
#include "defs.hpp"
#include "ast_types.hpp"
#include "print.hpp"

extern int parse_file(const File&, ast::Module&);

void parse(const Array<File>& files, ast::Unit& unit)
{
    Array<Thread> threads;
    u32 i = 0;

    for (const auto& file : files) {

        vprintln("Parsing: ", file);
        unit.modules.emplace_back(new ast::Module { file });
        threads.push_back(Thread { [&unit](const File& file, const u32 i) -> void {

            TRY_CATCH(parse_file(file, *unit.modules.at(i)));
        }, file, i });
        i++;
    }

    for (auto& thread : threads) {
        thread.join();
    }
    CHECKE();
}

void pipe(const Array<File>& files)
{
    ast::Unit unit;
    
    try {
        parse(files, unit);

        for (auto& mod : unit.modules) {
            println(*mod);
        }
    } catch (Error& error) {
        println(error);
    }

}