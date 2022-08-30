var Law = require("./french_law.js");
var Benchmark = require("benchmark");
const util = require("util");
var suite = new Benchmark.Suite();

function run_computation(log) {
  var result = Law.computeAllocationsFamiliales({
    currentDate: new Date(Date.UTC(2020, 04, 20)),
    children: [
      {
        id: 0,
        remunerationMensuelle: 0,
        dateNaissance: new Date(Date.UTC(2003, 02, 02)),
        priseEnCharge: "Effective et permanente",
        aDejaOuvertDroitAuxAllocationsFamiliales: true,
      },
      {
        id: 1,
        remunerationMensuelle: 300,
        dateNaissance: new Date(Date.UTC(2013, 09, 30)),
        priseEnCharge: "Garde alternée, partage des allocations",
        aDejaOuvertDroitAuxAllocationsFamiliales: true,
      },
    ],
    income: 30000,
    residence: "Métropole",
    personneQuiAssumeLaChargeEffectivePermanenteEstParent: true,
    personneQuiAssumeLaChargeEffectivePermanenteRemplitConditionsTitreISecuriteSociale: true,
    avaitEnfantAChargeAvant1erJanvier2012: false,
  });
  if (log) {
    console.log(
      util.inspect(Law.retrieveLog(0).slice(1, 10), {
        showHidden: false,
        depth: null,
        colors: true,
      })
    );
  }
  Law.resetLog();
}

// run_computation(true);
// process.exit(0);

suite
  .add("AllocationFamiliales#benchmark", function () {
    run_computation(false);
  })
  .on("cycle", function (event) {
    console.log(String(event.target));
  })
  .run({ async: true });
