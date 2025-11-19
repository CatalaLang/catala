# Guide for Catala releases

## Update version number on `catala-language-server`

Before tagging the release, update the version number in the `package.json` file.
Commit the change.

## Tag the release

On all three repos: catala, catala-language-server, catala-format.
Use semantic versionning; our policy for breaking changes is:
* if there's a breaking change in the syntax of the language, increment the
  major version.
* if there's a breaking change in the standard library, increment the
  minor version.

## Publish the release on Opam

On a clone of the [opam repository](https://github.com/ocaml/opam-repository/),
copy the `.opam` files from the previous release in the opam repository.
Then manually check the diff with the upstream opam files from this repo, and
propagate any difference.

The hashes should match the archives served by github, they can be obtained
using the following shell function (use `dl TAG REPO`)
```bash
dl() {
  curl -L "https://github.com/CatalaLang/$2/archive/refs/tags/$1.tar.gz" -o /tmp/xx.tgz;
  tar -tzf /tmp/xx.tgz |head &&
    echo '    "md5='$(md5sum /tmp/xx.tgz | cut -d' ' -f1)'"' &&
    echo '    "sha512='$(sha512sum /tmp/xx.tgz | cut -d' ' -f1)'"';
}
```

## Generate the debian package

From this `catala` repository

* If you're using `git`:
  - get your worktree on the release tag
  - `git merge pkgdeb` (locally, you should not ustream this merge!)
* If you're using `jj`: `jj new <TAG> pkgdeb`
* `make -f Makefile.debian debian-package`
* The generated file should be found in `../`. Upload it to the github release.


## Create Github releases

Add an entry to the [releases of Catala](https://github.com/CatalaLang/catala/releases):
* provide a link to the opam packages ;
* upload the relevant debian package and document it.

Add an entry to the [releases of `catala-language-server`](https://github.com/CatalaLang/catala-language-server/releases)

## Publish the VSCode extension on the VSCode Marketplace

Creating a catala-language-server release from a tag (as you did
in the previous step) will automatically publish the VSCode extension
on the marketplace.
You may check that it's happening smoothly
[here](https://github.com/CatalaLang/catala-language-server/actions).

After a few minutes, the new release should appear on
[this page](https://marketplace.visualstudio.com/items?itemName=catalalang.catala).
