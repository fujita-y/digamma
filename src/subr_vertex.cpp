// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "fiber.h"
#include "subr.h"

#include <google/cloud/aiplatform/v1/content.pb.h>
#include <google/cloud/aiplatform/v1/prediction_client.h>
#include <google/cloud/aiplatform/v1/prediction_connection.h>

#include <agrpc/asio_grpc.hpp>
#include <boost/fiber/all.hpp>
#include <google/cloud/aiplatform/v1/prediction_service.grpc.pb.h>
#include <grpcpp/grpcpp.h>
#include <memory>
#include <thread>
#include "asio.h"

#include <cstdlib>
#include <mutex>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace aiplatform = google::cloud::aiplatform_v1;

// ---------------------------------------------------------------------------
// GrpcContext background thread — drives the gRPC CompletionQueue.
// ---------------------------------------------------------------------------
struct GrpcContextOwner {
  agrpc::GrpcContext ctx;
  std::unique_ptr<boost::asio::executor_work_guard<agrpc::GrpcContext::executor_type>> work_guard;
  std::thread thread;

  GrpcContextOwner() {
    work_guard = std::make_unique<boost::asio::executor_work_guard<agrpc::GrpcContext::executor_type>>(boost::asio::make_work_guard(ctx));
    thread = std::thread([this]() { ctx.run(); });
  }

  ~GrpcContextOwner() {
    work_guard.reset();
    ctx.stop();
    if (thread.joinable()) {
      thread.join();
    }
  }
};

static agrpc::GrpcContext& get_grpc_context() {
  static GrpcContextOwner owner;
  return owner.ctx;
}

// ---------------------------------------------------------------------------
// fiber_await_vertex — yields fiber until Asio completion handler fires
// ---------------------------------------------------------------------------
template <typename R = grpc::Status, typename AsyncOp> static R fiber_await_vertex(AsyncOp&& op) {
  boost::fibers::promise<R> prom;
  auto fut = prom.get_future();
  std::forward<AsyncOp>(op)([p = std::move(prom)](const R& v, auto&&...) mutable { p.set_value(v); });
  return fut.get();
}

static std::shared_ptr<aiplatform::PredictionServiceClient> get_sync_client(const std::string& location) {
  static std::mutex mutex;
  static std::unordered_map<std::string, std::shared_ptr<aiplatform::PredictionServiceClient>> clients;
  std::lock_guard<std::mutex> lock(mutex);
  auto it = clients.find(location);
  if (it == clients.end()) {
    auto connection = aiplatform::MakePredictionServiceConnection(location);
    auto client = std::make_shared<aiplatform::PredictionServiceClient>(std::move(connection));
    it = clients.emplace(location, std::move(client)).first;
  }
  return it->second;
}

static std::shared_ptr<google::cloud::aiplatform::v1::PredictionService::Stub> get_async_stub(const std::string& location) {
  static std::mutex mutex;
  static std::unordered_map<std::string, std::shared_ptr<google::cloud::aiplatform::v1::PredictionService::Stub>> stubs;
  std::lock_guard<std::mutex> lock(mutex);
  std::string endpoint = location + "-aiplatform.googleapis.com";
  auto it = stubs.find(endpoint);
  if (it == stubs.end()) {
    auto channel = grpc::CreateChannel(endpoint, grpc::GoogleDefaultCredentials());
    auto stub = google::cloud::aiplatform::v1::PredictionService::NewStub(channel);
    it = stubs.emplace(endpoint, std::move(stub)).first;
  }
  return it->second;
}

// ---------------------------------------------------------------------------

SUBR subr_generate_content(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 4) {
    throw std::runtime_error("generate-content: wrong number of arguments (expected 1 to 4)");
  }

  if (!is_string(argv[0])) {
    throw std::runtime_error("generate-content: first argument must be a string (prompt)");
  }
  std::string prompt(reinterpret_cast<const char*>(string_name(argv[0])));

  std::string model_name = "gemini-1.5-pro";
  if (argc >= 2) {
    if (!is_string(argv[1])) {
      throw std::runtime_error("generate-content: second argument must be a string (model)");
    }
    model_name = reinterpret_cast<const char*>(string_name(argv[1]));
  }

  std::string location = "us-central1";
  if (argc >= 3) {
    if (!is_string(argv[2])) {
      throw std::runtime_error("generate-content: third argument must be a string (location)");
    }
    location = reinterpret_cast<const char*>(string_name(argv[2]));
  } else {
    const char* env_loc = std::getenv("GOOGLE_CLOUD_LOCATION");
    if (env_loc) {
      location = env_loc;
    }
  }

  std::string project_id = "";
  if (argc >= 4) {
    if (!is_string(argv[3])) {
      throw std::runtime_error("generate-content: fourth argument must be a string (project-id)");
    }
    project_id = reinterpret_cast<const char*>(string_name(argv[3]));
  } else {
    const char* env_proj = std::getenv("GOOGLE_CLOUD_PROJECT");
    if (env_proj) {
      project_id = env_proj;
    } else {
      throw std::runtime_error("generate-content: project-id not provided and GOOGLE_CLOUD_PROJECT environment variable not set");
    }
  }

  try {
    auto client = get_sync_client(location);

    std::string fully_qualified_model;
    if (model_name.find('/') != std::string::npos) {
      fully_qualified_model = model_name;
    } else {
      fully_qualified_model = "projects/" + project_id + "/locations/" + location + "/publishers/google/models/" + model_name;
    }

    google::cloud::aiplatform::v1::Content content;
    content.set_role("user");
    auto* part = content.add_parts();
    part->set_text(prompt);

    std::vector<google::cloud::aiplatform::v1::Content> contents;
    contents.push_back(std::move(content));

    auto response = client->GenerateContent(fully_qualified_model, contents);
    if (!response) {
      throw std::runtime_error("generate-content: Vertex AI API call failed: " + response.status().message());
    }

    std::string generated_text = "";
    for (int i = 0; i < response->candidates_size(); ++i) {
      const auto& candidate = response->candidates(i);
      const auto& candidate_content = candidate.content();
      for (int j = 0; j < candidate_content.parts_size(); ++j) {
        const auto& part_item = candidate_content.parts(j);
        if (part_item.has_text()) {
          generated_text += part_item.text();
        }
      }
    }

    return make_string(generated_text.c_str());

  } catch (const std::exception& e) {
    throw std::runtime_error("generate-content: " + std::string(e.what()));
  }
}

// ---------------------------------------------------------------------------
// subr_generate_content_async — (generate-content-async prompt [model [location [project]]]) → future
//
// Returns a Scheme future immediately; a detached fiber calls the synchronous
// Vertex AI stub on its own stack.  The promise is fulfilled on the main thread
// via normal fiber scheduling — exactly the same pattern as https-get-async in
// subr_net.cpp.  No background OS thread or cross-thread post needed.
// ---------------------------------------------------------------------------
SUBR subr_generate_content_async(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 1 || argc > 4) {
    throw std::runtime_error("generate-content-async: wrong number of arguments (expected 1 to 4)");
  }

  if (!is_string(argv[0])) {
    throw std::runtime_error("generate-content-async: first argument must be a string (prompt)");
  }
  std::string prompt(reinterpret_cast<const char*>(string_name(argv[0])));

  std::string model_name = "gemini-2.5-flash";
  if (argc >= 2) {
    if (!is_string(argv[1])) {
      throw std::runtime_error("generate-content-async: second argument must be a string (model)");
    }
    model_name = reinterpret_cast<const char*>(string_name(argv[1]));
  }

  std::string location = "us-central1";
  if (argc >= 3) {
    if (!is_string(argv[2])) {
      throw std::runtime_error("generate-content-async: third argument must be a string (location)");
    }
    location = reinterpret_cast<const char*>(string_name(argv[2]));
  } else {
    const char* env_loc = std::getenv("GOOGLE_CLOUD_LOCATION");
    if (env_loc) location = env_loc;
  }

  std::string project_id;
  if (argc >= 4) {
    if (!is_string(argv[3])) {
      throw std::runtime_error("generate-content-async: fourth argument must be a string (project-id)");
    }
    project_id = reinterpret_cast<const char*>(string_name(argv[3]));
  } else {
    const char* env_proj = std::getenv("GOOGLE_CLOUD_PROJECT");
    if (env_proj) {
      project_id = env_proj;
    } else {
      throw std::runtime_error("generate-content-async: project-id not provided and GOOGLE_CLOUD_PROJECT environment variable not set");
    }
  }

  // Allocate the future and GC-root it before the fiber is launched so the GC
  // cannot collect it between now and when the fiber fulfills the promise.
  scm_obj_t future_obj = make_future(scm_unspecified, nullptr);
  context::gc_protect(future_obj);

  // Package the synchronous work as a packaged_task so the shared_future can
  // be attached to the scm_future_rec_t before the fiber is scheduled.
  auto task = boost::fibers::packaged_task<scm_obj_t()>([future_obj, prompt, model_name, location, project_id]() mutable -> scm_obj_t {
    fiber_unwind_guard guard(future_obj);

    assert(context::s_asio_context && "generate-content-async requires an active Asio context");

    // Ensure GrpcContext is running
    get_grpc_context();

    std::string fully_qualified_model;
    if (model_name.find('/') != std::string::npos) {
      fully_qualified_model = model_name;
    } else {
      fully_qualified_model = "projects/" + project_id + "/locations/" + location + "/publishers/google/models/" + model_name;
    }

    auto stub = get_async_stub(location);

    google::cloud::aiplatform::v1::GenerateContentRequest request;
    request.set_model(fully_qualified_model);

    auto* content = request.add_contents();
    content->set_role("user");
    auto* part = content->add_parts();
    part->set_text(prompt);

    grpc::ClientContext client_context;
    google::cloud::aiplatform::v1::GenerateContentResponse response;

    grpc::Status status = fiber_await_vertex<grpc::Status>([&](auto h) {
      agrpc::ClientRPC<&google::cloud::aiplatform::v1::PredictionService::Stub::AsyncGenerateContent>::request(
          get_grpc_context(), *stub, client_context, request, response, boost::asio::bind_executor(context::s_asio_context->ctx, std::move(h)));
    });

    if (!status.ok()) {
      throw std::runtime_error("generate-content-async: Vertex AI API call failed: " + status.error_message());
    }

    std::string generated_text;
    for (int i = 0; i < response.candidates_size(); ++i) {
      const auto& candidate = response.candidates(i);
      const auto& candidate_content = candidate.content();
      for (int j = 0; j < candidate_content.parts_size(); ++j) {
        const auto& part_item = candidate_content.parts(j);
        if (part_item.has_text()) generated_text += part_item.text();
      }
    }

    return make_string(generated_text.c_str());
    // Any exception propagates into the future automatically.
  });

  auto* rec = static_cast<scm_future_rec_t*>(to_address(future_obj));
  rec->future = new boost::fibers::shared_future<scm_obj_t>(task.get_future().share());

  context::s_live_fiber_count++;
  boost::fibers::fiber(std::allocator_arg, context::s_fiber_stack_allocator, std::move(task)).detach();

  return future_obj;
}

void init_subr_vertex() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("generate-content", (void*)subr_generate_content, 1, true);
  reg("generate-content-async", (void*)subr_generate_content_async, 1, true);
}
