// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "fiber.h"
#include "subr.h"
#include "uniq_id.h"

#include <google/cloud/common_options.h>
#include <google/cloud/dialogflow/cx/v3/session.pb.h>
#include <google/cloud/dialogflow_cx/sessions_client.h>
#include <google/cloud/dialogflow_cx/sessions_connection.h>

#include <agrpc/asio_grpc.hpp>
#include <boost/fiber/all.hpp>
#include <google/cloud/dialogflow/cx/v3/session.grpc.pb.h>
#include <grpcpp/grpcpp.h>
#include <memory>
#include <thread>
#include "asio.h"

#include <cstdlib>
#include <mutex>
#include <stdexcept>
#include <string>
#include <unordered_map>

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
// fiber_await — yields fiber until Asio completion handler fires
// ---------------------------------------------------------------------------
template <typename R = grpc::Status, typename AsyncOp> static R fiber_await(AsyncOp&& op) {
  boost::fibers::promise<R> prom;
  auto fut = prom.get_future();
  std::forward<AsyncOp>(op)([p = std::move(prom)](const R& v, auto&&...) mutable { p.set_value(v); });
  return fut.get();
}

static std::shared_ptr<google::cloud::dialogflow_cx::SessionsClient> get_sync_client(const std::string& location) {
  static std::mutex mutex;
  static std::unordered_map<std::string, std::shared_ptr<google::cloud::dialogflow_cx::SessionsClient>> clients;
  std::lock_guard<std::mutex> lock(mutex);
  auto it = clients.find(location);
  if (it == clients.end()) {
    google::cloud::Options options;
    if (location != "global") {
      options.set<google::cloud::EndpointOption>(location + "-dialogflow.googleapis.com");
    }
    auto connection = google::cloud::dialogflow_cx::MakeSessionsConnection(location, options);
    auto client = std::make_shared<google::cloud::dialogflow_cx::SessionsClient>(std::move(connection));
    it = clients.emplace(location, std::move(client)).first;
  }
  return it->second;
}

static std::shared_ptr<google::cloud::dialogflow::cx::v3::Sessions::Stub> get_async_stub(const std::string& location) {
  static std::mutex mutex;
  static std::unordered_map<std::string, std::shared_ptr<google::cloud::dialogflow::cx::v3::Sessions::Stub>> stubs;
  std::lock_guard<std::mutex> lock(mutex);
  std::string endpoint;
  if (location == "global") {
    endpoint = "dialogflow.googleapis.com";
  } else {
    endpoint = location + "-dialogflow.googleapis.com";
  }
  auto it = stubs.find(endpoint);
  if (it == stubs.end()) {
    auto channel = grpc::CreateChannel(endpoint, grpc::GoogleDefaultCredentials());
    auto stub = google::cloud::dialogflow::cx::v3::Sessions::NewStub(channel);
    it = stubs.emplace(endpoint, std::move(stub)).first;
  }
  return it->second;
}

// ---------------------------------------------------------------------------

SUBR subr_dialogflow_cx_detect_intent(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 2 || argc > 6) {
    throw std::runtime_error("dialogflow-cx-detect-intent: wrong number of arguments (expected 2 to 6)");
  }

  if (!is_string(argv[0])) {
    throw std::runtime_error("dialogflow-cx-detect-intent: first argument must be a string (text)");
  }
  std::string text(reinterpret_cast<const char*>(string_name(argv[0])));

  if (!is_string(argv[1])) {
    throw std::runtime_error("dialogflow-cx-detect-intent: second argument must be a string (agent-id)");
  }
  std::string agent_id(reinterpret_cast<const char*>(string_name(argv[1])));

  std::string session_id = generate_uuid();
  if (argc >= 3) {
    if (!is_string(argv[2])) {
      throw std::runtime_error("dialogflow-cx-detect-intent: third argument must be a string (session-id)");
    }
    session_id = reinterpret_cast<const char*>(string_name(argv[2]));
  }

  std::string location = "us-central1";
  if (argc >= 4) {
    if (!is_string(argv[3])) {
      throw std::runtime_error("dialogflow-cx-detect-intent: fourth argument must be a string (location)");
    }
    location = reinterpret_cast<const char*>(string_name(argv[3]));
  } else {
    const char* env_loc = std::getenv("GOOGLE_CLOUD_LOCATION");
    if (env_loc) location = env_loc;
  }

  std::string project_id = "";
  if (argc >= 5) {
    if (!is_string(argv[4])) {
      throw std::runtime_error("dialogflow-cx-detect-intent: fifth argument must be a string (project-id)");
    }
    project_id = reinterpret_cast<const char*>(string_name(argv[4]));
  } else {
    const char* env_proj = std::getenv("GOOGLE_CLOUD_PROJECT");
    if (env_proj) {
      project_id = env_proj;
    } else {
      throw std::runtime_error("dialogflow-cx-detect-intent: project-id not provided and GOOGLE_CLOUD_PROJECT environment variable not set");
    }
  }

  std::string language_code = "en";
  if (argc >= 6) {
    if (!is_string(argv[5])) {
      throw std::runtime_error("dialogflow-cx-detect-intent: sixth argument must be a string (language-code)");
    }
    language_code = reinterpret_cast<const char*>(string_name(argv[5]));
  }

  try {
    auto client = get_sync_client(location);

    std::string session_path = "projects/" + project_id + "/locations/" + location + "/agents/" + agent_id + "/sessions/" + session_id;

    google::cloud::dialogflow::cx::v3::DetectIntentRequest request;
    request.set_session(session_path);
    auto* query_input = request.mutable_query_input();
    auto* text_input = query_input->mutable_text();
    text_input->set_text(text);
    query_input->set_language_code(language_code);

    auto response = client->DetectIntent(request);
    if (!response) {
      throw std::runtime_error("dialogflow-cx-detect-intent: Dialogflow CX API call failed: " + response.status().message());
    }

    std::string generated_text = "";
    for (int i = 0; i < response->query_result().response_messages_size(); ++i) {
      const auto& msg = response->query_result().response_messages(i);
      if (msg.has_text()) {
        for (int j = 0; j < msg.text().text_size(); ++j) {
          if (!generated_text.empty()) generated_text += "\n";
          generated_text += msg.text().text(j);
        }
      }
    }

    return make_string(generated_text.c_str());

  } catch (const std::exception& e) {
    throw std::runtime_error("dialogflow-cx-detect-intent: " + std::string(e.what()));
  }
}

SUBR subr_dialogflow_cx_detect_intent_async(scm_obj_t self, int argc, scm_obj_t argv[]) {
  if (argc < 2 || argc > 6) {
    throw std::runtime_error("dialogflow-cx-detect-intent-async: wrong number of arguments (expected 2 to 6)");
  }

  if (!is_string(argv[0])) {
    throw std::runtime_error("dialogflow-cx-detect-intent-async: first argument must be a string (text)");
  }
  std::string text(reinterpret_cast<const char*>(string_name(argv[0])));

  if (!is_string(argv[1])) {
    throw std::runtime_error("dialogflow-cx-detect-intent-async: second argument must be a string (agent-id)");
  }
  std::string agent_id(reinterpret_cast<const char*>(string_name(argv[1])));

  std::string session_id = generate_uuid();
  if (argc >= 3) {
    if (!is_string(argv[2])) {
      throw std::runtime_error("dialogflow-cx-detect-intent-async: third argument must be a string (session-id)");
    }
    session_id = reinterpret_cast<const char*>(string_name(argv[2]));
  }

  std::string location = "us-central1";
  if (argc >= 4) {
    if (!is_string(argv[3])) {
      throw std::runtime_error("dialogflow-cx-detect-intent-async: fourth argument must be a string (location)");
    }
    location = reinterpret_cast<const char*>(string_name(argv[3]));
  } else {
    const char* env_loc = std::getenv("GOOGLE_CLOUD_LOCATION");
    if (env_loc) location = env_loc;
  }

  std::string project_id = "";
  if (argc >= 5) {
    if (!is_string(argv[4])) {
      throw std::runtime_error("dialogflow-cx-detect-intent-async: fifth argument must be a string (project-id)");
    }
    project_id = reinterpret_cast<const char*>(string_name(argv[4]));
  } else {
    const char* env_proj = std::getenv("GOOGLE_CLOUD_PROJECT");
    if (env_proj) {
      project_id = env_proj;
    } else {
      throw std::runtime_error(
          "dialogflow-cx-detect-intent-async: project-id not provided and GOOGLE_CLOUD_PROJECT environment variable not set");
    }
  }

  std::string language_code = "en";
  if (argc >= 6) {
    if (!is_string(argv[5])) {
      throw std::runtime_error("dialogflow-cx-detect-intent-async: sixth argument must be a string (language-code)");
    }
    language_code = reinterpret_cast<const char*>(string_name(argv[5]));
  }

  scm_obj_t future_obj = make_future(scm_unspecified, nullptr);
  context::gc_protect(future_obj);

  auto task = boost::fibers::packaged_task<scm_obj_t()>([future_obj, text, agent_id, session_id, location, project_id,
                                                         language_code]() mutable -> scm_obj_t {
    fiber_unwind_guard guard(future_obj);

    assert(context::s_asio_context && "dialogflow-cx-detect-intent-async requires an active Asio context");

    get_grpc_context();

    std::string session_path = "projects/" + project_id + "/locations/" + location + "/agents/" + agent_id + "/sessions/" + session_id;

    auto stub = get_async_stub(location);

    google::cloud::dialogflow::cx::v3::DetectIntentRequest request;
    request.set_session(session_path);
    auto* query_input = request.mutable_query_input();
    auto* text_input = query_input->mutable_text();
    text_input->set_text(text);
    query_input->set_language_code(language_code);

    grpc::ClientContext client_context;
    google::cloud::dialogflow::cx::v3::DetectIntentResponse response;

    grpc::Status status = fiber_await<grpc::Status>([&](auto h) {
      agrpc::ClientRPC<&google::cloud::dialogflow::cx::v3::Sessions::Stub::AsyncDetectIntent>::request(
          get_grpc_context(), *stub, client_context, request, response, boost::asio::bind_executor(context::s_asio_context->ctx, std::move(h)));
    });

    if (!status.ok()) {
      throw std::runtime_error("dialogflow-cx-detect-intent-async: Dialogflow CX API call failed: " + status.error_message());
    }

    std::string generated_text = "";
    for (int i = 0; i < response.query_result().response_messages_size(); ++i) {
      const auto& msg = response.query_result().response_messages(i);
      if (msg.has_text()) {
        for (int j = 0; j < msg.text().text_size(); ++j) {
          if (!generated_text.empty()) generated_text += "\n";
          generated_text += msg.text().text(j);
        }
      }
    }

    return make_string(generated_text.c_str());
  });

  auto* rec = static_cast<scm_future_rec_t*>(to_address(future_obj));
  rec->future = new boost::fibers::shared_future<scm_obj_t>(task.get_future().share());

  context::s_live_fiber_count++;
  boost::fibers::fiber(std::allocator_arg, context::s_fiber_stack_allocator, std::move(task)).detach();

  return future_obj;
}

void init_subr_dialogflow() {
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("dialogflow-cx-detect-intent", (void*)subr_dialogflow_cx_detect_intent, 2, true);
  reg("dialogflow-cx-detect-intent-async", (void*)subr_dialogflow_cx_detect_intent_async, 2, true);
}
