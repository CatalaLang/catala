# Guide for Catala releases

## Tag the release

on all three repos: catala, catala-language-server, catala-format

## handle the opam version

on a clone of the opam repository, copy opam files from the previous release, and check the diff with the upstream opam files.

The hashes should match the archives served by github, they can be obtained using the following shell function (use `dl TAG REPO`)
```bash
dl() {
  curl -L "https://github.com/CatalaLang/$2/archive/refs/tags/$1.tar.gz" -o /tmp/xx.tgz;
  tar -tzf /tmp/xx.tgz |head &&
    echo '    "md5='$(md5sum /tmp/xx.tgz | cut -d' ' -f1)'"' &&
    echo '    "sha512='$(sha512sum /tmp/xx.tgz | cut -d' ' -f1)'"';
}
```

## do a github release

it may be enough to do it on catala.

## generate the debian package

On the catala repo:
- git:
  - get your worktree on the release tag
  - `git merge pkgdeb` (locally, you should not ustream this merge!)
  jj: `jj new <TAG> pkgdeb`
- `make -f Makefile.debian debian-package`
- the generated file should be found in `../`. Upload it to the github release.
