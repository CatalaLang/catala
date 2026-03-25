v1.2

* Fixed streaming RPCs in servers to be closed as soon as the clients
  leave (instead of waiting for the next flush)
* Added the possibility to specify a conn_closed hook function at the
  creation of servers

v1.1

* fix: The client do not wait for a first element of a stream to arrive.

v1.0

* refactoring: Invert control over resto server
* feature: client follow redirects

v0.10:

feature: Merge for dynamic and dynamic tails directories

v0.9:

* feature: Added a strategy to pick on the left or on the right on a directory merging collision.
* feature: Added printers for `Conflict` exception.

0.8:

* Improve ocaml version coverage in CI
* Added helpers for pretty-printing Paths
* Added service-lookup feature to resolve parametric URI from concrete URI

v0.7:

* Remove Ezresto (and other Ez libraries)
* Packaging, build, tests and CI improvements
* Add the `middleware` feature (contributed by Swann Moreau)

v0.6/v0.6.1:

* Split path before percentage-decoding chunks to allow slash's encoding to
  appear in chunks
* Added ACL module to allow/deny access to entry points based on path matching
* The boolean parameter now accepts an empty string, in which case the value is 
  considered `true`
* Added support for self-serving client
* Added support for chunking answers
* Allow to specify agent-string
* More tests and better documentation
* More logging with better level settings

v0.5:

* Documentation
* Updated dependencies (notably Lwt)

v0.4:

* client does not depend on Unix anymore. Users must pass `gettimeofday`
  function directly.
* Added `Gone response code

v0.3:

* Schemas are now lazy (to speed up initialisation and because schemas are not
  always used) (by Romain)
- depend on json-data-encoding (new) fork of ocplib-json-typed (deprecated)

v0.2:

- fork from ocplib-resto: new home, new CI, etc.

