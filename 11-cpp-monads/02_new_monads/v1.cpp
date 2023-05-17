// Verzija 1 - kod sa casa
// U haskelu smo imali Maybe, Either i listu kao primere funktora i monada,
// ovde imamo odgovarajuce pandane: std::optional, std::expected, std::vector
// za koje hocemo da pokazemo da su funktori i monade, tj. implementiramo fmap i mbind.
// Pored toga, pravimo koncepte functor i monad po ugledu na klase Functor i Monad iz haskela.
// https://en.cppreference.com/w/cpp/concepts
// U C++23 ubacene su monadicke operacije za std::optional i std::expected
// https://en.cppreference.com/w/cpp/utility/optional#Monadic_operations

#include <iostream>
#include <optional>
#include <vector>
#include <numeric>
#include <concepts>
#include <set>
#include <expected>
#include <algorithm>
#include <functional>

// data Expected T E = Value T | Error E 

// (Ord a) => a -> b

template <typename A, typename Function>
requires std::invocable<Function, A>
std::optional<std::invoke_result_t<Function, A>> fmap(const std::optional<A> &opt, Function f)
{
    if(opt) {
        return f(opt.value());
    } else {
        return {};
    }
}

template <typename A, typename E, typename Function>
std::expected<std::invoke_result_t<Function, A>, E> fmap(const std::expected<A, E> &exp, Function f)
{
    if(exp) {
        return f(exp.value());
    } else {
        return exp;
    }
}

// >>= :: m a -> (a -> m b) -> m b
template <typename A, typename Function>
requires std::invocable<Function, A>
std::invoke_result_t<Function, A> mbind(const std::optional<A> &opt, Function f)
{
    if(opt) {
        return f(opt.value());
    } else {
        return {};
    }
}

template <typename A, typename E, typename Function>
std::invoke_result_t<Function, A> mbind(const std::expected<A, E> &exp, Function f)
{
    if(exp) {
        return f(exp.value());
    } else {
        return exp;
    }
}

template <typename A, typename Function>
requires std::invocable<Function, A>
std::vector<std::invoke_result_t<Function, A>> fmap(const std::vector<A> &xs, Function f)
{
    std::vector<std::invoke_result_t<Function, A>> result;
    std::transform(xs.begin(), xs.end(),
                   std::back_inserter(result),
                   f);
    return result;
}

template <typename A, typename Function>
requires std::invocable<Function, A>
std::invoke_result_t<Function, A> mbind(const std::vector<A> &xs, Function f)
{
    using B = std::invoke_result_t<Function, A>::value_type;
    std::vector<std::invoke_result_t<Function, A>> ys;
    std::transform(xs.begin(), xs.end(),
                   std::back_inserter(ys),
                   f);
    return std::accumulate(ys.begin(), ys.end(),
                           std::vector<B>(),
                           [](const auto &acc, const auto &x){
                            std::invoke_result_t<Function, A> r(acc);
                            r.insert(r.end(), x.begin(), x.end());
                            return r;
                           });
}

template <typename ResFunctor, typename OurFunctor, typename B>
concept fmap_result = requires (ResFunctor f, OurFunctor f2, B b) {
    requires std::same_as<decltype(convert_value_type_to_int(f)),
                          decltype(convert_value_type_to_int(f2))> &&
    std::same_as<typename ResFunctor::value_type, B>;
};

// std::function<B(A)>

// f a
// f::value_type
template <typename Functor, typename Function, typename A = Functor::value_type>
concept functor = requires (Functor f, Function g)
{
    requires std::invocable<Function, A>;
    {fmap(f, g)} -> fmap_result<Functor, std::invoke_result_t<Function, A>>;
};

template <typename Monad>
auto convert_value_type_to_int (Monad f)
{
    return fmap(f, [](auto){return 0;});
}

template <typename ResMonad, typename OurMonad, typename B>
concept mbind_result = requires (ResMonad f, OurMonad f2, B b) {
    requires std::same_as<decltype(convert_value_type_to_int(f)),
                          decltype(convert_value_type_to_int(f2))> &&
    std::same_as<typename ResMonad::value_type, typename B::value_type>;
};

template <typename Monad, typename Function, typename A = Monad::value_type>
concept monad = requires (Monad f, Function g)
{
    requires std::invocable<Function, A>;
    {mbind(f, g)} -> mbind_result<Monad, std::invoke_result_t<Function, A>>;
};

template <typename Functor, typename Function>
requires functor<Functor, Function>
auto fmap_twice(Functor f, Function g)
{
    auto mid = fmap(f, g);
    return fmap(mid, g);
}

int increment(int x)
{
    return x + 1;
}

std::vector<int> repeat_2(int x)
{
    return {x,x};
}

std::vector<int> repeat_3(int x)
{
    return {x,x,x};
}

// >>=
template <typename Monad, typename Function, typename A = Monad::value_type>
requires monad<Monad, Function>
std::invoke_result_t<Function, A> operator|(const Monad &xs, Function f)
{
    return mbind(xs, f);
}

// m a >>= \ _ -> m b = m b
template <typename Monad1, typename Monad2>
Monad2 operator>>(const Monad1 &m1, const Monad2 &m2)
{
    return m1 | [&m2](auto){return m2;};
}

std::expected<double, std::string> smart_f(int x)
{
    if(x % 2 == 0) {
        return 2.0 * x;
    } else {
        return std::unexpected("neparan broj...");
    }
}

int main()
{
    std::optional<int> opt {12};
    // auto res = fmap_twice(opt, increment);
    // auto res = mbind(opt, [](auto x){std::optional<int> r {x*x}; return r;});
    // auto res = fmap(opt, increment)
    //                | :::;
    auto res = opt.transform(increment)
                  .and_then([](auto x){std::optional<int> r {x*x}; return r;});
    // auto res = opt | smart_f; // greska
    if(res) {
        std::cout << res.value() << std::endl;
    } else {
        std::cout << "empty optional" << std::endl;
    }

    std::vector<int> xs {1,2,3};
    // auto v_res = fmap_twice(xs, increment);
    // auto v_res = mbind(mbind(xs, repeat_2), repeat_3);
    // xs >>= repeat_2 >>= repeat3
    // auto v_res = xs | repeat_2 | [](int x){return x*x;};
    auto v_res = xs | repeat_2 | repeat_3;
    for(const auto &x : v_res) {
        std::cout << x << ' ';
    }
    std::cout << std::endl;

    // std::set s {1,2,3};
    // auto res_s = s | [](int x){return std::set {x,x};};

    std::optional<int> x {1};
    std::optional<int> y {2};
    auto z = x >> y;
    if(z) {
        std::cout << z.value() << std::endl;
    } else {
        std::cout << "empty optional" << std::endl;
    }

    std::vector<int> ys {4,5,6};
    auto zs = xs >> ys;
    for(const auto &x : zs) {
        std::cout << x << ' ';
    }
    std::cout << std::endl;

    std::expected<int, std::string> exp {4};
    // std::expected<int, std::string> exp = std::unexpected("neka greska");
    // auto exp_res = fmap_twice(exp, increment);
    auto exp_res = exp | smart_f
                       | [](double x){return std::expected<double, std::string>{x + 5.7};};
    if(exp_res) {
        std::cout << "value: " << exp_res.value() << std::endl;
    } else {
        std::cout << "error: " << exp_res.error() << std::endl;
    }


    // std::set<int> s1{1,2,3};
    // std::set<int> s2{4,5,6};
    // auto res_s = s1 >> s2;

 
    return 0;
}