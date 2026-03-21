#include "core.h"
#include "nanos_options.h"

std::string nanos_options::boot_file = "../boot/core.ir";
std::string nanos_options::env_name = "system";
std::string nanos_options::script_file = "";

void nanos_options::parse(int argc, char** argv) {
  CLI::App app{"Nanos"};
  argv = app.ensure_utf8(argv);
  app.add_option("--boot", boot_file, "Specify the boot filename");
  app.add_option("--env", env_name, "Specify the environment name (system or interaction)");
  app.add_option("--script,scriptfile", script_file, "Specify the script filename");
  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& e) {
    app.exit(e);
  }
}
