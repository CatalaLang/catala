FROM docker.io/ocaml/opam:debian-12-ocaml-5.0

USER root

RUN apt-get update && apt-get install -y git ca-certificates libgmp-dev pkg-config ninja-build openjdk-17-jdk && rm -rf /var/lib/apt/lists/*

USER opam
# rust setup (for catala-format)
ENV RUSTUP_VERSION=1.28.2
ENV RUST_VERSION=1.88.0

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
    sh -s -- -y --default-toolchain ${RUST_VERSION} --profile minimal

ENV PATH="/home/opam/.cargo/bin:${PATH}"

RUN opam pin catala-lsp.dev git+https://github.com/CatalaLang/catala-language-server -y
RUN opam pin catala-format.dev git+https://github.com/CatalaLang/catala-format -y

CMD ["/bin/bash"]
