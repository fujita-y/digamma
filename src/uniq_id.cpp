#include <atomic>
#include <format>
#include <random>
#include <thread>

#include "uniq_id.h"

std::string generate_process_unique_suffix() {
  static std::atomic<uint32_t> counter = 1;
  uint32_t seq = counter.fetch_add(1, std::memory_order_relaxed);
  uint32_t thread = std::hash<std::thread::id>{}(std::this_thread::get_id());
  return std::format("0{:x}_{:x}", seq, thread);
}

std::string generate_uuid() {
  static thread_local std::random_device rd;
  static thread_local std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 15);
  std::uniform_int_distribution<> dis_variant(8, 11);

  std::string uuid(36, ' ');
  const char* hex = "0123456789abcdef";
  for (int i = 0; i < 36; i++) {
    if (i == 8 || i == 13 || i == 18 || i == 23) {
      uuid[i] = '-';
    } else if (i == 14) {
      uuid[i] = '4';
    } else if (i == 19) {
      uuid[i] = hex[dis_variant(gen)];
    } else {
      uuid[i] = hex[dis(gen)];
    }
  }
  return uuid;
}
