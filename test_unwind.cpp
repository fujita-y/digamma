#include <iostream>
#include <boost/context/continuation.hpp>
namespace ctx = boost::context;
int main() {
  ctx::continuation k = ctx::callcc([](ctx::continuation&& sink) {
    try {
      sink = sink.resume();
    } catch (const ctx::detail::forced_unwind& e) {
      std::cout << "Caught forced_unwind!\n";
      throw;
    } catch (...) {
      std::cout << "Caught something else!\n";
    }
    return std::move(sink);
  });
  std::cout << "Main: destroying k\n";
  k = ctx::continuation();
  std::cout << "Main: done\n";
}
