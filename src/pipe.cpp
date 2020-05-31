#include "std.hpp"
#include "defs.hpp"
#include "ast_types.hpp"
#include "print.hpp"

extern int parse_file(const File&, Fn<void(ast::Module*)>);

void parse(const Array<File>& files, ast::Unit& unit)
{
    Mutex mutex;
    u32 i = 0;

    for (const auto& file : files) {

        vprintln("Parsing: ", file);
        THREAD_POOL.dispatch( { [&unit, &mutex, &file]() -> void {

            parse_file(file, [&unit, &mutex](ast::Module* mod) {
                mutex.lock();
                unit.modules.emplace_back(mod);
                mutex.unlock();
            });
        } });
        i++;
    }

    THREAD_POOL.wait();
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