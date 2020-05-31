#include "defs.hpp"
#include <fstream>
#include <iomanip>

// CONFIG

const _Config CONFIG = _Config {
    .version = "0.0.1",
    .cc = "gcc",
    .verbose = false,
    .thread_count = 4,
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

    os << error.msg;

    return os;
}

// THREAD POOL

void _Thread_Pool::dispatch(Fn<void(void)> fn)
{
    {
        std::lock_guard<Mutex> lck { count_mutex };
        count++;
        printf("increasing work count %d\n", count);
    }

    {
        std::lock_guard<Mutex> lck { mutex };
        std::lock_guard<Mutex> lckd { data_mutex };
        printf("pushing work\n");
        fn_queue.push(move(fn));
    }
    //vprintln("Work ready");
    cond.notify_one();
}

void _Thread_Pool::wait()
{
    std::unique_lock<Mutex> lck { count_mutex };
    printf("waiting for all threads to finish\n");
    cond_count.wait(lck, [&] { return count == 0; });
    CHECKE();
    printf("waiting done\n");
}

void _Thread_Pool::init(u32 _thread_count)
{
    running.store(true);
    for (u32 i = 0; i < _thread_count; i++) {
        threads.emplace_back([&] (u32 i) { _work(i); }, i);
    }
}

void _Thread_Pool::cleanup()
{
    running.store(false);
    cond.notify_all();
    for (auto& thread : threads) {
        thread.join();
    }
    printf("cleanup done\n");
}

void _Thread_Pool::_work(int i)
{
    printf("thread starting\n");
    (void) i;
    Fn<void(void)> l_data_fn;
    while (running.load()) {
        //vprintln("Thread[", i, "] waiting for work");
        printf("thread %d waiting for work\n", i);

        std::unique_lock<Mutex> lck { mutex };
        cond.wait(lck, [&] { 

            std::lock_guard<Mutex> lckd { data_mutex };
            bool res = fn_queue.size() >= 1;

            if (res) {
                l_data_fn = move(fn_queue.front());
                fn_queue.pop();
            }
            return res || !running.load();
        });

        if (std::lock_guard<Mutex> lck { count_mutex };
                !running.load() && count == 0) {
            printf("thread %d done\n", i);
            return;
        }

        printf("thread %d starting work\n", i);
        //vprintln("Thread[", i, "] starting work");
        TRY_CATCH(l_data_fn());

        {
            std::lock_guard<Mutex> lck { count_mutex };
            count--;
            printf("decreasing count %d\n", count);
            cond_count.notify_all();
        }
    }
}

_Thread_Pool THREAD_POOL {};