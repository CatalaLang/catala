# Javascript French Law Library

This folder contains a ready-to-use Javascript library featuring French public
algorithms coded up in Catala.

## Generating the source files

The JS code is extracted from OCaml using
[`js_of_ocaml`](https://ocsigen.org/js_of_ocaml/). See the
[dedicated README](../ocaml/README.md) of the OCaml library for more precisions
about the OCaml code.

The wrapping between OCaml and JS is done by the generated
`../ocaml/law_source/<filename>_api_web.ml` modules.

You can generate the `french_law.js` source JS module by invoking this command
from the root of the repository:

```
make build_french_law_library_js
```

## API description

The `french_law.js` library exposes:

- an [event manager](#the-event-manager)
- a list of [API functions](#api-functions)
- a list of fully exposed [sub-libraries](#sub-libraries)

### The event manager

A JavaScript object `eventsManager` is exposed with three callable methods:

```javascript
var frenchLaw = require("french_law.js");

// Clears the raw log event array.
frenchLaw.eventsManager.resetLog(0);

// Returns the current content of the raw log event array.
let rawEvents = frenchLaw.eventsManager.retrieveRawEvents(0)

// Returns the event array parsed from the current content of the raw log event array.
let events = frenchLaw.eventsManager.retrieveEvents(0)
```

> **Important**: you need to give an arbitrary value as argument.

### API functions

The `french_law.js` library exposes for each Catala program available in
`../ocaml/law_source/` a function to call in order to run the corresponding
encoded algorithm.

#### Available algorithms

##### Allocations familiales

The function is `computeAllocationsFamiliales`. This computation
returns the amount of _allocations familiales_ for one household described
by the input. More precisely, the result returned is the sum of:

- _la base des allocations familiales_
- _l'allocation forfaitaire relai pour dépassement de l'âge limite_
- _la majoration pour âge_
- _le complément dégressif en cas de dépassement du plafond de revenus_

An example of use:

```javascript
var frenchLaw = require("french_law.js");

let amount = frenchLaw.computeAllocationsFamiliales({
  iDateCouranteIn: new Date("2020-05-20"),
  iEnfantIn: [
    {
      dIdentifiant: 0,
      dRemunerationMensuelle: 0.0,
      dDateDeNaissance: new Date("2003-03-02"),
      dPriseEnCharge: "EffectiveEtPermanente",
      dADejaOuvertDroitAuxAllocationsFamiliales: true,
      dBeneficieTitrePersonnelAidePersonnelleLogement: false,
    },
    {

      dIdentifiant: 1,
      dRemunerationMensuelle: 330.0,
      dDateDeNaissance: new Date("2013-10-30"),
      dPriseEnCharge: "GardeAlterneePartageAllocations",
      dADejaOuvertDroitAuxAllocationsFamiliales: true,
      dBeneficieTitrePersonnelAidePersonnelleLogement: false,
    },
  ],
  iRessourcesMenageIn: 30000.0,
  iResidenceIn: "Metropole",
  iPersonneChargeEffectivePermanenteEstParentIn: true,
  iPersonneChargeEffectivePermanenteRemplitTitreIIn: true,
  iAvaitEnfantAChargeAvant1erJanvier2012In: true,
});
```

Notably, the `dPriseEnCharge` variable for each child expects a value among:
- "GardeAlterneePartageAllocations"
- "GardeAlterneeAllocataireUnique"
- "EffectiveEtPermanente"
- "ServicesSociauxAllocationVerseeALaFamille"
- "ServicesSociauxAllocationVerseeAuxServicesSociaux"

> See `../ocaml/law_source/allocations_familiales_api_web.ml` for more
> information about data types.

### Sub libraries

All declared types and scopes of a Catala program are available in JavaScript
via the following sub libs:

```javascript
var frenchLaw = require("french_law.js");

// Allocations familiales
// corresponding to the file: `../ocaml/law_source/allocations_familiales_api_web.ml
var allocationsFamiliales = frenchLaw.AllocationsFamilialesLib
```
