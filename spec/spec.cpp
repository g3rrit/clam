#include <functional>
#include <memory>
#include <iostream>

template <class T>
T undefined() {
	std::cout << "error" << std::endl;
	exit(-1);
	return std::declval<T>();
}

// data A = B Int | C

struct D0 {
	enum {
		D1,
		D2,
	} _type;
	union {
		struct {
			int D1_1;
		};
	};
};

std::function<D0(int)> ConD_1 = [](int a) {
	return D0 { D0::D1 , 10 };
};

// let foo a : Int -> Int = a + a
std::function<int(int)> foo = [](int a) {
	return a + a;
};


// let bar a : Int -> Int -> Int = bar

std::function<std::function<int(int)>(int)> bar = [](int  a) {
	return foo;
};

// let etest a b : A -> Int -> Int
//   = a  -- EVar
//   ; 10 -- EPrim
//   ; bar 10 2 -- EApp
//   ; f : Int -> Int = \x -> x * x -- ELet ELam
//   ; if 0 then 1 else >> -- EIf
//   ; match a with 
//       B y -> y * 2
//       C   -> 2

std::function<std::function<int(int)>(D0)> etest = [](D0 a) {
	return [=](int b) {
		std::function<int(int)> f;

		return 
			( a
			, ( 10
			  , ( bar(10)(2) 
			  	, ( f = [=] (int x) { return x * x; }
				  , ( 0 ? 1 
				  	  : ( a._type == D0::D1 ? a.D1_1 * 2 
						  : (a._type == D0::D2 ? 2 : undefined<int>())
						)
					)
				  )
				)
			  )
			);

		/*
		return 
			( a
			, 10
			, bar(10)(2)
			, f = [=](int x) { return x * x; }
			, ( 0 ? 1 : 
				( a._type == A::B ? a.B_1 * 2 :
					( a._type == A::C ? 2 : undefined<int>()
					)
				)
		 	  )
			);
	 	*/
	};
};

// -- if a lambda captures an unique ptr it also needs to become an unique ptr
// let uptest p a : ^Int -> ^(Int -> Int)
//   = p + a

/*
std::function<std::function<int(int)>(std::unique_ptr<int>)> uptest = [] (std::unique_ptr<int> p) {
	return std::bind([] (std::unique_ptr<int> ps, int a) {
		return *ps + a;
	}, std::move(p));
};
*/

int main() {
	std::cout << bar(10)(20) << std::endl;
	std::cout << etest(ConD_1(10))(10) << std::endl;
	// std::cout << uptest(std::move(std::make_unique<int>(1)))(2) << std::endl;
	return 0;
}