#include "core.h"
#include "nanos_options.h"

std::string nanos_options::boot_file = "/home/digamma/github/digamma/boot/core.ir";
std::string nanos_options::env_name = "interaction";
std::string nanos_options::script_file = "";
std::vector<std::string> nanos_options::load_paths = {"~/github/digamma/stdlib"};

void nanos_options::parse(int argc, char** argv) {
  CLI::App app{"Nanos"};
  argv = app.ensure_utf8(argv);
  app.add_option("--boot", boot_file, "Specify the boot filename");
  app.add_option("--env", env_name, "Specify the environment name ('system' or 'interaction')");
  app.add_option("--script", script_file, "Specify the script filename");
  app.add_option("--load-path", load_paths, "Specify the load paths (':' separated)")->delimiter(':');
  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& e) {
    app.exit(e);
  }
}
