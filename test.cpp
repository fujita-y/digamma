#include <iostream>
__attribute__((no_sanitize("hwaddress"))) static void f1() {}
static __attribute__((no_sanitize("hwaddress"))) void f2() {}
static void __attribute__((no_sanitize("hwaddress"))) f3() {}
static void f4() __attribute__((no_sanitize("hwaddress"))) {}
