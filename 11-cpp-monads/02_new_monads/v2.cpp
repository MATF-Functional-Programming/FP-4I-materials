// Verzija 2 - autor Aleksandar Zecevic
// 1. umesto da Functor bude obican parametar kao u v1, sada je template parametar
//    koncepti functor i monad postaju jednostavniji
// 2. umesto da Function bude obican parametar kao u v1, sada je std::function<B(A)>
//    malo ruznije koriscenje u main-u jer nemamo automatsku konverziju u std::function, ali
//    opet malo jednostavniji kod koncepata (nemamo std::invoke_result_t i slicno)

#include <algorithm>
#include <concepts>
#include <functional>
#include <iostream>
#include <numeric>
#include <optional>
#include <set>
#include <string>
#include <type_traits>
#include <vector>

// template<typename F, typename A>
// requires std::regular_invocable<F, A>
// std::optional<std::invoke_result_t<F, A>> fmap(F f, std::optional<A> a)
// {
//   if (a)
//     return f(a.value());
//   else
//     return {};
// }

template<typename A, typename B>
std::optional<B> fmap(const std::function<B(A)>& f, const std::optional<A>& a)
{
  if (a)
    return f(a.value());
  else
    return {};
}

template<typename A, typename B>
std::optional<B> mbind(const std::optional<A>& a, const std::function<std::optional<B>(A)>& f)
{
  if (a)
    return f(a.value());
  else
    return {};
}

// template<typename F, typename A>
// requires std::regular_invocable<F, A>
// std::vector<std::invoke_result_t<F, A>> fmap(F f, std::vector<A> a)
// {
//   std::vector<std::invoke_result_t<F, A>> res;
//   std::transform(a.begin(), a.end(), std::back_inserter(res), f);
//   return res;
// }

template<typename A, typename B>
std::vector<B> fmap(const std::function<B(A)>& f, const std::vector<A>& a)
{
  std::vector<B> res;
  std::transform(a.begin(), a.end(), std::back_inserter(res), f);
  return res;
}

template<typename A, typename B>
std::vector<B> mbind(const std::vector<A>& a, const std::function<std::vector<B>(A)>& f)
{
  return std::accumulate(a.begin(), a.end(),
                         std::vector<B>{},
                         [&f](std::vector<B>&& acc, const A& a) {
                                auto r = f(a);
                                acc.insert(acc.end(), r.begin(), r.end());
                                return acc;
                              } 
                        );
}

template<template<typename> typename F, typename A, typename B>
concept Functor = requires(std::function<B(A)> f, F<A> a) {
  { fmap(f, a) } -> std::same_as<F<B>>;
};

template<template<typename> typename F, typename A, typename B, typename C>
requires Functor<F, A, B> && Functor<F, B, C>
F<C> fmap_compose(const std::function<C(B)>& g, const std::function<B(A)>& f, const F<A>& a)
{
  return fmap(g, fmap(f, a));
}

template<template<typename> typename F, typename A>
requires Functor<F, A, A>
F<A> fmap_twice(const std::function<A(A)>& f, const F<A>& a)
{
  return fmap_compose(f, f, a);
}

template<template<typename> typename M, typename A, typename B>
concept Monad = Functor<M, A, B> &&
  requires(std::function<M<B>(A)> f, M<A> m, A a) {
    { mbind(m, f) } -> std::same_as<M<B>>;
  };

template<template<typename> typename M, typename A, typename B>
requires Monad<M, A, B>
M<B> operator |(const M<A>& m, const std::function<M<B>(A)>& f)
{
  return mbind(m, f);
}

// alternativa za monade
// template<template<typename> typename M, typename A, typename Function, typename B = std::invoke_result_t<Function, A>::value_type>
// concept Monad = Functor<M, A, B> &&
//   requires(Function f, M<A> m, A a) {
//     { mbind(m, f) } -> std::same_as<M<B>>;
//   };

// template<template<typename> typename M, typename A, typename Function>
// requires std::regular_invocable<Function, A> && Monad<M, A, Function>
// std::invoke_result_t<Function, A> operator |(const M<A>& m, const Function& f)
// {
//   return mbind(m, f);
// }

int increment(int x)
{
  return x + 1;
}

double half(int x)
{
  return x * 0.5;
}

std::vector<int> replicate_odd(int a)
{
  if (a % 2)
    return { a, a };
  else
    return { a };
}

std::vector<int> replicate_even(int a)
{
  if (a % 2)
    return { a };
  else
    return { a, a };
}

template<typename A>
void print(const std::optional<A>& opt)
{
  if (opt)
    std::cout << opt.value() << std::endl;
  else
    std::cout << "empty optional" << std::endl;
}

template<typename A>
void print(const std::vector<A>& vec)
{
  for (const auto& x : vec)
    std::cout << x << " ";
  std::cout << std::endl;
}

int main()
{
  std::optional opt{ 42 };
  print(fmap(std::function{ increment }, opt));
  print(fmap(std::function{ increment }, std::optional<int>{ std::nullopt }));
  print(fmap_compose(std::function{ half }, std::function{ increment }, opt));
  print(fmap_twice(std::function{ increment }, opt));
  // fmap_compose(std::function{ increment }, std::function{ half }, std::optional{ 42 });
  print(mbind(opt, std::function{ [](int x) { return std::optional{ x + 1 }; }}));
  print(mbind(std::optional<int>{ std::nullopt }, std::function{ [](int x) { return std::optional{ x + 1 }; }}));

  std::vector vec { 1, 2, 3 };
  print(fmap(std::function { increment }, vec));
  print(fmap(std::function{ increment }, std::vector<int>{}));
  print(fmap_compose(std::function{ half }, std::function{ increment }, vec));
  // fmap_twice(std::function{ half }, vec);
  print(mbind(vec, std::function { replicate_even }));
  print(vec | std::function{ replicate_even } | std::function{ replicate_odd });
  print(vec | std::function{ replicate_odd } | std::function{ replicate_odd } | std::function{ replicate_even });
  print(
    vec |
    std::function{ replicate_even } |
    std::function{ [](int x) { return std::vector{ std::to_string(x) }; }}
  );

  // std::set set { 1, 2, 3 };
  // fmap_twice(std::function{ increment }, set);
  // set | std::function{ [](int x) { return std::set{ x }; }};

  return 0;
}