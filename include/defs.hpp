#ifndef DEF_HPP
#define DEF_HPP

#include "std.hpp"
#include <exception>
#include <stdexcept>
#include <condition_variable>
#include <atomic>

// CONFIG

struct Config {
    String version;
    String cc;
    bool   verbose;
    u32    thread_count;

    String output;
};

extern Config CONFIG;

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

struct Thread_Pool {

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

    Fn<void(u32)> _worker_fn = [&] (int i) {
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
    };

    void dispatch(Fn<void(void)> fn)
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

    void wait() {
        std::unique_lock<Mutex> lck { count_mutex };
        printf("waiting for all threads to finish\n");
        cond_count.wait(lck, [&] { return count == 0; });
        CHECKE();
        printf("waiting done\n");
    }

    void init(u32 _thread_count)
    {
        running.store(true);
        for (u32 i = 0; i < _thread_count; i++) {
            threads.emplace_back(_worker_fn, i);
        }
    }

    void cleanup()
    {
        running.store(false);
        cond.notify_all();
        for (auto& thread : threads) {
            thread.join();
        }
        printf("cleanup done\n");
    }
};

extern Thread_Pool THREAD_POOL;

#endif