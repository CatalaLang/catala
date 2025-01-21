# escape=`

FROM mcr.microsoft.com/dotnet/framework/sdk:4.8.1

USER ContainerAdministrator
LABEL distro_style="windows"
SHELL ["powershell", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]

RUN Set-ExecutionPolicy Bypass -Scope Process -Force
RUN Set-ExecutionPolicy Bypass -Scope CurrentUser -Force
RUN [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
RUN iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

RUN choco install ninja zip unzip -y

ENV CYGWIN="nodosfilewarning winsymlinks:native"

RUN Invoke-RestMethod https://opam.ocaml.org/install.ps1 | Invoke-Expression

# 1337 H@CK
RUN opam init --bare -y --disable-sandboxing --shell=powershell -k local ;`
    cd C:\Users\ContainerAdministrator\AppData\Local\opam\ ;`
    zip -r cygwin.zip .\.cygwin ; rm -r -fo .\.cygwin ; unzip cygwin.zip ; rm -fo cygwin.zip

RUN opam switch create 4.14.1

RUN opam pin add catala-lsp git+https://github.com/CatalaLang/catala-language-server -y

# Install & setup rust
RUN choco install rustup.install git mingw -y
RUN rustup toolchain install stable-x86_64-pc-windows-gnu ; `
    rustup default stable-x86_64-pc-windows-gnu

# Install: catala-format + remove dead link
RUN opam install conf-rust-2021.1 -y
RUN opam install catala-format -y ; `
    rm C:\Users\ContainerAdministrator\AppData\Local\opam\4.14.1\share\topiary\queries\ocaml_interface.scm

# Packaging

## nsis
RUN choco install nsis -y
ADD https://nsis.sourceforge.io/mediawiki/images/7/7f/EnVar_plugin.zip C:\
RUN unzip C:\EnVar_plugin.zip -d 'C:\Program Files (x86)\NSIS'

WORKDIR C:\packaging\
ADD static\* C:\packaging\
RUN cp C:\Users\ContainerAdministrator\AppData\Local\opam\.cygwin\root\usr\x86_64-w64-mingw32\sys-root\mingw\bin\libgmp-10.dll .

## Pulling latest catala version
WORKDIR C:\
RUN git clone https://github.com/CatalaLang/catala ; git config --global --add safe.directory C:/catala
RUN git clone https://github.com/CatalaLang/catala-language-server ; git config --global --add safe.directory C:/catala-language-server
WORKDIR C:\catala
RUN (& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ } ; git cherry-pick --no-commit 51772e98ae35071c3fdec01041078533672395b1 ; opam install . --deps-only -y ; dune build compiler/catala.exe
RUN cp C:\catala\_build\default\compiler\catala.exe C:\packaging\
WORKDIR C:\catala-language-server
RUN (& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ } ; opam install . --working-dir --assume-depexts --inplace-build --keep-build-dir -y
RUN cp _build\default\server\src\main.exe C:\packaging\catala-lsp.exe

## building catala-setup.exe
WORKDIR C:\packaging
RUN & 'C:\Program Files (x86)\NSIS\makensis.exe' .\catala-setup.nsi
