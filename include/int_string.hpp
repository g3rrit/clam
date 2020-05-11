#ifndef INT_STRING_H
#define INT_STRING_H

#include <cstring>
#include <cstdlib>

struct string {
	char *_str = nullptr;

	string() {}

	string(char const * const s) {
		_str = new char[std::strlen(s) + 1];
		std::strcpy(_str, s);
	}

	string(const string& s) 
	 	: string(s._str) {}

	auto operator=(const string& s) -> string& {
		return *this = string(s);
	}

	string(string&& s) {
		_str = s._str;
		s._str = nullptr;
	}

	auto operator=(string&& s) -> string& {
		_str = s._str;
		s._str = nullptr;
		return *this;
	}

	~string() {
		if (_str != nullptr) {
			delete _str;
		}
	}
};

#endif