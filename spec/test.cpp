
#include <iostream>
#include <functional>


template <class T, class K>
auto add(T a) -> std::function<int(K)>
{
	return [=] (T b) -> T { return a + b; };
}

auto main() -> int 
{
	auto r = add(10);

	std::cout << "res: " << r(20) << std::endl;

	return 0;
}
