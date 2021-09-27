FROM ocaml/opam:ubuntu-lts-ocaml-4.12

RUN sudo apt-get update && sudo apt-get install -y \
    man2html \
    colordiff \
    latexmk \
    python3 \
    python3-pip \
    libgmp-dev \
    npm \
    nodejs

RUN sudo pip3 install virtualenv
