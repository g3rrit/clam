#ifndef MEM_HPP
#define MEM_HPP

#include <utility>

namespace mem {

    template <class T>
    T* alloc()
    {
        return new T {};
    }

    template <class T, class... Args>
    T* alloc(Args&&... args)
    {
        return new T { std::forward(args)... };
    }

    template <class T>
    void free(T* t)
    {
        if (t != nullptr) {
            free(t);
        }
    }

    template <class T, class... Args>
    void free(T* t, Args... args)
    {
        free(t);
        free(args...);
    }

}

#endif