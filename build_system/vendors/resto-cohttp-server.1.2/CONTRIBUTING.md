
# Contributing to resto 

## Coding guidelines

* TODO cross build compatibility rules

## Contribution guidelines

* TODO commit name conventions
* TODO merging/rebasing conventions / linear history good practices

## Release guidelines

As a component owner having the right permissions over the repository, 
you will need to use the following instructions to release a new version
of `resto` into `opam`:

1. Communicate with involved parties to avoid merges on `master` during
   the releasing process. This is not critical and merging can still be 
   required depending on the case, but it will permit to keep a clean
   linear history.

2. Run the following commands where `<XXX>` represents the new version number:
   ```bash
   git checkout master
   git pull origin master 
   git checkout -b release-v<XXX>
   ```

3. Update the `CHANGES.md` file by prepending the new version and a list
   of features and fixes introduced since last version.
   
4. Generate a token from https://github.com/settings/tokens/new and make sure
   to check the "public_repo" scope box.

5. Run the following commands:
   ```bash
   git commit -am "CHANGES v<XXX>"
   git tag v<XXX>
   git push origin v<XXX>
   opam publish "https://gitlab.com/nomadic-labs/resto/-/archive/v<XXX>/resto-v<XXX>.tar.gz" .
   ```
 
6. When the prompt is requiring it, past your token and press enter.

7. `opam publish` will open a pull request on https://github.com/ocaml/opam-repository.
   Resolve all threads until opam repository members decide to merge.
   
8. After the MR has been merged, the release branch can be merged on master.
   **Make sure not to rebase the branch before merging** to preserve tag consistency.
   If the merge can't fast-forward because some branches has been merged during the 
   releasing process, it is OK to use a merge commit for release branches.
   
