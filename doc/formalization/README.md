# Certification of a critical Catala compilation step

## Presentation

This folder contains the F* proof of the correctness of the translation from the default calculus
to the target lambda calculus. The proof is organized as follows:

* `Catala.DefaultCalculus.fst` contains the semantics of the default calculus as well as the
  type safety theorems and proofs;
* `Catala.LambdaCalculus.fst` contains the semantics of the lambda calculus as well as the
  type safety theorems and proofs;
* `Catala.Translation.Helpers.fst` contains various helpers related to the lambda calculus,
  used in the translation correctness proof;
* `Catala.Translation.fst` contains the definition of the translation as well as its proof
  of correctness, culminating in the last wrap-up theorem of the file.

## Proof replay

To replay the proofs, you will need to have [F*](https://github.com/FStarLang/FStar) installed
on your machine, and the `FSTAR_HOME` environement variable pointed to the location of the F*
directory. You can then replay the proofs using:

    make verify

The proofs should take 4~5 minutes to replay entirely. Warning: due to Z3 non-deterministic behavior,
some proofs may fail to replay on your machine for unknown reasons. May this happen, we will make
sure to strenghen the flaky proofs for the camera-ready version of the artefact.

## Disclaimer

This mechanization contains two distinct admitted lemmas, all related to administrative aspects of
the target lambda calculus. Both are in `Catala.TranslationHelpers.fst`.

* The first admit concerns `well_typed_terms_invariant_by_subst`, which is a classic result stating
  that correclty typed terms with no free variables are invariant by substitution. The proof in F*
  needs a version of context invariance different from the one we used to prove type safety, hence
  this temporary admit.
* The second assumed hypothesis in inside `lift_multiple_l_steps_exceptions_head`, and is related
  two lambda calculus terms, `init0` and `init2`. `init2` is the result of the double application
  of the beta-reduction to `init0`. Both terms are reduced versions of the big, deeply embedded
  term `process_exceptions`, which causes F* automation a lot of troubles. The difficulty with
  the proof seems to stem from the instantiations of `well_typed_terms_invariant_by_subst`, which
  is missing some key step.

We plan of course to prove those two hypothesis in due time for an upcoming artefact, and hope these
explanations did convince you that the rest of the proof remains solidly grounded.