FROM gitpod/workspace-full

USER gitpod

ENV PATH "/usr/lib/llvm-11/bin:${PATH}"
ENV C_INCLUDE_PATH "/usr/lib/llvm-11/include"
ENV CPLUS_INCLUDE_PATH "/usr/lib/llvm-11/include"

# Install custom tools, runtime, etc. using apt-get
# For example, the command below would install "bastet" - a command line tetris clone:
#
# RUN sudo apt-get -q update && sudo apt-get install -yq bastet && sudo rm -rf /var/lib/apt/lists/*
#
# More information: https://www.gitpod.io/docs/config-docker/
