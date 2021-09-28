# The Catala formalization

The formalization of Catala is presented in the paper [Catala: A Programming Language for the Law](https://arxiv.org/abs/2103.03198).

# Certification of a critical Catala compilation step

## Presentation

This folder contains the F\* proof of the correctness of the translation from the default calculus
to the target lambda calculus. The proof is organized as follows:

- `Catala.DefaultCalculus.fst` contains the semantics of the default calculus as well as the
  type safety theorems and proofs;
- `Catala.LambdaCalculus.fst` contains the semantics of the lambda calculus as well as the
  type safety theorems and proofs;
- `Catala.Translation.Helpers.fst` contains various helpers related to the lambda calculus,
  used in the translation correctness proof;
- `Catala.Translation.fst` contains the definition of the translation as well as its proof
  of correctness, culminating in the last wrap-up theorem of the file.

The `*.hints` files are merely F\* technical helper files that speed up the proof replay by recording
a list of lemmas to feed in priority to the SMT solver.

## Proof replay

To replay the proofs, you will need to have [F\*](https://github.com/FStarLang/FStar) installed
on your machine, and the `FSTAR_HOME` environement variable pointed to the location of the F\*
directory. You can then replay the proofs using:

    make verify

The proofs should take 4~5 minutes to replay entirely. Warning: due to Z3 non-deterministic behavior,
some proofs may fail to replay on your machine for unknown reasons. May this happen, we will make
sure to strenghen the flaky proofs for the camera-ready version of the artefact.

## Disclaimer

This mechanization contains one admitted lemma, related to administrative aspects of
the target lambda calculus. It is in `Catala.TranslationHelpers.fst`.

The admit concerns `well_typed_terms_invariant_by_subst`, which is a classic result stating
that correclty typed terms with no free variables are invariant by substitution. The proof in F\*
needs a version of context invariance different from the one we used to prove type safety, hence
this temporary admit.
