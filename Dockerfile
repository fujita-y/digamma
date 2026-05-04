ARG UBUNTU_VERSION=24.04
FROM ubuntu:${UBUNTU_VERSION}

ARG LLVM_VERSION
ENV LLVM_VERSION=${LLVM_VERSION}
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    wget gnupg lsb-release software-properties-common \
    cmake ninja-build pkg-config curl zip unzip tar git \
    && rm -rf /var/lib/apt/lists/*

RUN wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key | gpg --dearmor -o /usr/share/keyrings/llvm-archive-keyring.gpg && \
    CODENAME=$(lsb_release -cs) && \
    echo "deb [signed-by=/usr/share/keyrings/llvm-archive-keyring.gpg] http://apt.llvm.org/${CODENAME}/ llvm-toolchain-${CODENAME}-${LLVM_VERSION} main" | tee /etc/apt/sources.list.d/llvm.list && \
    apt-get update && apt-get install -y \
    llvm-${LLVM_VERSION} \
    llvm-${LLVM_VERSION}-dev \
    clang-${LLVM_VERSION} \
    lld-${LLVM_VERSION} \
    libclang-${LLVM_VERSION}-dev && \
    update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-${LLVM_VERSION} 100 && \
    update-alternatives --install /usr/bin/clang clang /usr/bin/clang-${LLVM_VERSION} 100 && \
    update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-${LLVM_VERSION} 100 && \
    update-alternatives --install /usr/bin/ld.lld ld.lld /usr/bin/ld.lld-${LLVM_VERSION} 100 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

RUN git clone https://github.com/microsoft/vcpkg.git /vcpkg --depth 1 && \
    /vcpkg/bootstrap-vcpkg.sh -disableMetrics

RUN cmake -S . -B build \
    -G Ninja \
    -DCMAKE_TOOLCHAIN_FILE=/vcpkg/scripts/buildsystems/vcpkg.cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_CONFIG_CMD=/usr/bin/llvm-config-${LLVM_VERSION} && \
    cmake --build build --parallel $(nproc)

CMD ["/app/build/nanos", "--boot=/app/boot/core.ir", "--load-path=/app/stdlib"]
