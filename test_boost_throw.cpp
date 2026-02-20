#include <iostream>
#include <boost/context/continuation.hpp>

int main() {
  try {
    throw boost::context::detail::forced_unwind();
  } catch (const boost::context::detail::forced_unwind& e) {
    std::cout << "Caught forced_unwind!\n";
  } catch (...) {
    std::cout << "Caught something else!\n";
  }
}
