## Style conventions for the standard library

* Test functions returning a boolean should be named `is_...` or `est_...`,
  or `are_...` or `sont_...` when there are multiple arguments.
* Verbs in French should be in imperative form and not infinitive form.
* The description of the function should speak about the function at the third
  person: "Computes" instead of "Compute", etc.
* For docstring starting by `##` :
    * Code inserts should be surrounded by anti-quotes (Markdown style)
    * The name of arguments can be referred to directly since they're part of
      the prototype.
    * The docstring should have the following form:

```text
## Main message describing what is the goal of the function. New sentence with
## additional information.
## **Example(s):** Unique example here or
## * First example
## * Second example
## **Aborts:** List of input conditions for which the function crashes.
```

```text
## Message principal décrivant le but de la fonction. Nouvelle
## phrase avec des informations additionnelles.
## **Exemple(s):** Exemple unique ici ou
## * Premier exemple
## * Deuxième exemple
## **Échoue:** Liste de conditions sur les entrées telles que la fonction
## crashe.
```
