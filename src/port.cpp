// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"

#include <fstream>   // IWYU pragma: keep
#include <iostream>  // IWYU pragma: keep
#include <istream>   // IWYU pragma: keep
#include <ostream>   // IWYU pragma: keep
#include <sstream>   // IWYU pragma: keep
#include <variant>

void port_finalize(scm_port_rec_t* rec) {
  port_aux_t* aux = rec->aux;
  if (aux == nullptr) return;
  if (aux->owned) {
    std::visit(
        [](auto&& arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (!std::is_same_v<T, std::monostate>) {
            if (arg != nullptr) {
              delete arg;
            }
          }
        },
        aux->stream);
  }
  aux->stream = std::monostate{};
  delete aux;
  aux = nullptr;
}