# The French Law Javascript Library

This Javascript library contains some computations defined by French
legislative texts. The JS code is extracted from OCaml, which is itself
extracted from Catala code (https://catala-lang.org).

## Allocations familiales

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
