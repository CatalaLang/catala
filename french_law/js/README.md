# Javascript French Law Library

This folder contains a ready-to-use Javascript library featuring French public
algorithms coded up in Catala.

## Generating the source files

The JS code is extracted from OCaml using
[`js_of_ocaml`](https://ocsigen.org/js_of_ocaml/). See the
[dedicated README](../ocaml/README.md) of the OCaml library for more precisions
about the OCaml code. The wrapping between OCaml and JS is done by the
`api_web.ml` module.

You can generate the `french_law.js` source JS module by invoking this command
from the root of the repository:

```
make build_french_law_library_js
```

## API description

<details>

<summary>API content</summary>

### Data types

#### `source_position`

Stores information about the log position.

```javascript
{
  fileName : string
  startLine : int
  endLine : int
  startColumn : int
  endColumn : int
  lawHeadings : [string]
 }
```

#### `log_event`

Stores information about one log event.

```javascript
{
  eventType : string
  information : [string]
  sourcePosition : source_position?
  loggedValueJson : string
}
```

`loggedValueJson` is the resulting string representation of the
`Runtime.runtime_value` serialization via `yojson` -- see
`./src/compiler/runtime.mli`.

### Functions

#### `french_law.resetLog()`

Clears the log array.

Example of use:

```javascript
french_law.resetLog(0) // Note that you have to pass an arbitrary argument.
```

#### `french_law.retrieveLog()`

Returns the array of [`log_event`](#log_event) resulting of the computation of the
[algorithms](#available-algorithms).

Example of use:

```javascript
let logs = french_law.retrieveLog(0) // Note that you have to pass an arbitrary argument.
```

</details>

## Available algorithms

### Allocations familiales

The function of the library is `computeAllocationsFamiliales`. This computation
returns the amount of _allocations familiales_ for one household described
by the input. More precisely, the result returned is the sum of:

- _la base des allocations familiales_
- _l'allocation forfaitaire relai pour dépassement de l'âge limite_
- _la majoration pour âge_
- _le complément dégressif en cas de dépassement du plafond de revenus_

An example of use:

```javascript
Law.computeAllocationsFamiliales({
  currentDate: new Date("2020-05-20"),
  children: [
    {
      id: 0,
      remunerationMensuelle: 0,
      dateNaissance: new Date("2003-03-02"),
      priseEnCharge: "Effective et permanente",
      aDejaOuvertDroitAuxAllocationsFamiliales: true,
    },
    {
      id: 1,
      remunerationMensuelle: 300,
      dateNaissance: new Date("2013-10-30"),
      priseEnCharge: "Garde alternée, partage des allocations",
      aDejaOuvertDroitAuxAllocationsFamiliales: true,
    },
  ],
  income: 30000,
  residence: "Métropole",
  personneQuiAssumeLaChargeEffectivePermanenteEstParent: true,
  personneQuiAssumeLaChargeEffectivePermanenteRemplitConditionsTitreISecuriteSociale: true,
});
```

Notably, the `priseEnCharge` variable for each child expects a value among:

- `"Effective et permanente"`
- `"Garde alternée, allocataire unique"`
- `"Garde alternée, partage des allocations"`
- `"Confié aux service sociaux, allocation versée à la famille"`
- `"Confié aux service sociaux, allocation versée aux services sociaux"`
