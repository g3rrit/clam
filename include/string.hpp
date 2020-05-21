#ifndef STRING_H
#define STRING_H

#include <cstring>
#include <cstdlib>

struct String {
    char *_str = nullptr;

    String() {}

    String(char const * const s)
    {
        _str = new char[std::strlen(s) + 1];
        std::strcpy(_str, s);
    }

    String(const String& s) 
         : String(s._str) {}

    String& operator=(const String& s)
    {
        return *this = String(s);
    }

    String(String&& s)
    {
        _str = s._str;
        s._str = nullptr;
    }

    String& operator=(String&& s)
    {
        _str = s._str;
        s._str = nullptr;
        return *this;
    }

    ~String()
    {
        if (_str != nullptr) {
            delete _str;
        }
    }

    char* c_str() const
    {
        return this->_str;
    }

    friend std::ostream& operator<<(std::ostream& os, const String& str)
    {
        if (str._str != nullptr) {
            os << str._str;
        }
        return os;
    }
};

#endif