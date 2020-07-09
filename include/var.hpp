#ifndef VAR_HPP
#define VAR_HPP

#include <variant>
#include <tuple>

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
    template <class T, class F>                                     \
    T visit(F&& f)                                                  \
    {                                                               \
        struct _F : F {                                             \
            using F::operator();                                    \
            T operator()(const std::monostate&)                     \
                {                                                   \
                    if constexpr (std::is_same<T, void>::value) {   \
                        return;                                     \
                    } else {                                        \
                        return *(T*)0; }                            \
                    }                                               \
        };                                                          \
        _F* _f = static_cast<_F*>(&f);                                 \
        return std::visit(*_f, var);                                 \
    }

#endif