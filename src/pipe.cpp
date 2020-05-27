#include "std.hpp"
#include "defs.hpp"
#include "ast_types.hpp"
#include "print.hpp"

extern int parse_file(const File&, Fn<void(ast::Module*)>);

void parse(const Array<File>& files, ast::Unit& unit)
{
    Array<Thread> threads;
    Mutex mutex;
    u32 i = 0;

    for (const auto& file : files) {

        vprintln("Parsing: ", file);
        threads.push_back(Thread { [&unit, &mutex](const File& file) -> void {

            TRY_CATCH(parse_file(file, [&unit, &mutex](ast::Module* mod) {
                mutex.lock();
                unit.modules.emplace_back(mod);
                mutex.unlock();
            }));
        }, file});
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