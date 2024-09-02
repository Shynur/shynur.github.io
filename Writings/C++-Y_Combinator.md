# `C++` 写 $\mathrm{Y}$ 组合子

定义: $\mathrm{Y}=\lambda f.(\lambda x.f(xx))(\lambda x.f(xx))$.
先给出 `Lisp` 形式, 方便改写成 `C++`.

```commonlisp
;; lisp
(defvar Y #'(lambda (f)
              ((lambda (g) (funcall g g)) #'(lambda (x)
                                              (funcall f #'(lambda (&rest args)
                                                             (apply (funcall x x) args)))))))
```

```C++
// cpp
template<typename return_t, typename ... arguments_t> auto Y = [](const auto& f) {
    return [](const auto& g) {return g(g);} ([&f](const auto& x) -> std::function<return_t(arguments_t...)> {
        return f([&x](arguments_t ... args) {return x(x)(args ...);});
    });
};
```

使用 gcc 或 msvc c++20 通过编译.<br> 
下面演示一下:

```C++
#include <functional> // for Y using function

template<typename return_t, typename ... arguments_t> auto Y = [](const auto& f) {
    return [](const auto& g) {return g(g);} ([&f](const auto& x) -> std::function<return_t(arguments_t ...)> {
        return f([&x](arguments_t ... args) {return x(x)(args ...);});
    });
};

#include <iostream> // for main to show some results

int main() {
    auto fib = Y<size_t, uint8_t, size_t, size_t>([](const auto& f) {
        return [&f](uint8_t n, size_t a, size_t b) {
            return n == 0 ? a : f(--n, b, a + b);
        };
    });

    for (auto i{0}; i != 10; ++i)
        std::cout << fib(i, 0, 1) << ' ';
}
```

输出:

```
0 1 1 2 3 5 8 13 21 34 
```

___

&copy; 2022 小烧杯 \<<one.last.kiss@outlook.com>\>. All rights reserved.
