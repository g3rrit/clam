#ifndef DEF_HPP
#define DEF_HPP

#include "std.hpp"
#include <exception>
#include <stdexcept>
#include <condition_variable>
#include <atomic>

// CONFIG

struct _Config {
    String version;
    String cc;
    bool   verbose;
    u32    thread_count;

    String output;
};

extern const _Config CONFIG;

// EXCEPTION

extern std::exception_ptr _EPTR;

static inline void CHECKE() {
    if (_EPTR != nullptr) {
        std::rethrow_exception(_EPTR);
    }
}

#define TRY_CATCH(x) \
    do { try { x; } catch (...) { \
        if (_EPTR == nullptr) { _EPTR = std::current_exception(); }  \
    } } while(0)

// PIPE

void pipe(const Array<File>& files);

// THREAD POOL

struct _Thread_Pool {

    Array<Thread> threads;

    std::atomic<bool> running;

    bool data_ready = false;

    Mutex mutex;
    std::condition_variable cond;

    Mutex data_mutex;
    Queue<Fn<void(void)>> fn_queue;

    Mutex count_mutex;
    u32 count;
    std::condition_variable cond_count;

    void dispatch(Fn<void(void)> fn);
    void wait();
    void init(u32 _thread_count);
    void cleanup();

    void _work(int i);
};

extern _Thread_Pool THREAD_POOL;

#endif