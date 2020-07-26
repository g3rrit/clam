#ifndef VAR_HPP
#define VAR_HPP

#include <variant>
#include <tuple>

using std::visit;

template<class... Ts> struct overload : Ts... { using Ts::operator()...; };
template<class... Ts> overload(Ts...) -> overload<Ts...>;

#define MAKE_STRUCT_X(cons, ...)                                    \
    struct cons : public std::tuple< __VA_ARGS__ > {                \
        using std::tuple< __VA_ARGS__ >::tuple;                     \
    };

#define MAKE_STRUCT_Y(cons)


#define GET_TYPES_X(cons, ...)                                      \
    cons ,

#define GET_TYPES_Y(cons)                                           \
    cons ,

#define MAKE_VARIANT(TYPE, LIST)                                    \
    LIST(MAKE_STRUCT_X, MAKE_STRUCT_Y)                              \
    std::variant< LIST(GET_TYPES_X, GET_TYPES_Y) std::monostate > var; \
    template<class T>                                               \
    TYPE(T& t) : var(t) {}                                          \
    template<class T>                                               \
    TYPE(T&& t) : var(t) {}                                         \
    template <class R, class Vis>                                   \
    friend R visit(Vis&& vis, TYPE&& t)                 \
    {                                                               \
        return std::visit<R, Vis, std::variant< LIST(GET_TYPES_X, GET_TYPES_Y) std::monostate >>(vis, t.var);                                \
    }


#endif