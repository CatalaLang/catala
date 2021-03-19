var Law = require("./french_law.js");
var Benchmark = require("benchmark");
var suite = new Benchmark.Suite();

suite
  .add("AllocationFamiliales#benchmark", function () {
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
  })
  .on("cycle", function (event) {
    console.log(String(event.target));
  })
  .run({ async: true });
