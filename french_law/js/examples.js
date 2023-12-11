var Law = require("./src/french_law.js");
var Benchmark = require("benchmark");
const util = require("util");
const { cachedDataVersionTag } = require("v8");
const { truncateSync } = require("fs");
var suite = new Benchmark.Suite();

function run_computation_AF(log) {
  var result = Law.computeAllocationsFamiliales({
    iDateCouranteIn: "2020-04-20",
    iEnfantsIn: [
      {
        dIdentifiant: 0,
        dRemunerationMensuelle: 0,
        dDateDeNaissance: "2003-02-02",
        dPriseEnCharge: { kind: "EffectiveEtPermanente", payload: null },
        dADejaOuvertDroitAuxAllocationsFamiliales: true,
        dBeneficieTitrePersonnelAidePersonnelleLogement: false,
      },
      {
        dIdentifiant: 1,
        dRemunerationMensuelle: 300,
        dDateDeNaissance: "2013-09-30",
        dPriseEnCharge: {
          kind: "GardeAlterneePartageAllocations",
          payload: null,
        },
        dADejaOuvertDroitAuxAllocationsFamiliales: true,
        dBeneficieTitrePersonnelAidePersonnelleLogement: false,
      },
    ],
    iRessourcesMenageIn: 30000,
    iResidenceIn: { kind: "Metropole", payload: null },
    iPersonneChargeEffectivePermanenteEstParentIn: true,
    iPersonneChargeEffectivePermanenteRemplitTitreIIn: true,
    iAvaitEnfantAChargeAvant1erJanvier2012In: false,
  });
  if (log) {
    console.log(
      util.inspect(Law.eventsManager.retrieveEvents(0).slice(1, 10), {
        showHidden: false,
        depth: null,
        colors: true,
      }),
    );
  }
  Law.eventsManager.resetLog();
  console.log("Family benefits mounthly amount:", result, "€");
}

function run_computation_AL(log) {
  var result = Law.computeAidesAuLogement({
    menageIn: {
      prestationsRecues: [],
      logement: {
        residencePrincipale: true,
        estEhpadOuMaisonAutonomieL31312Asf: false,
        modeOccupation: {
          kind: "Locataire",
          payload: {
            bailleur: {
              kind: "BailleurPrive",
            },
            beneficiaireAideAdulteOuEnfantHandicapes: false,
            logementEstChambre: false,
            colocation: false,
            ageesOuHandicapAdultesHebergeesOnereuxParticuliers: false,
            logementMeubleD8422: false,
            changementLogementD8424: {
              kind: "PasDeChangement",
              payload: null,
            },
            loyerPrincipal: 450,
          },
        },
        proprietaire: {
          kind: "Autre",
          payload: null,
        },
        loueOuSousLoueADesTiers: {
          kind: "Non",
        },
        usufruit: {
          kind: "Autre",
          payload: null,
        },
        logementDecentL89462: true,
        zone: {
          kind: "Zone2",
        },
        surfaceMCarres: 65,
      },
      personnesACharge: [
        {
          kind: "EnfantACharge",
          payload: {
            aDejaOuvertDroitAuxAllocationsFamiliales: true,
            remunerationMensuelle: 0,
            nationalite: {
              kind: "Francaise",
              payload: null,
            },
            etudesApprentissageStageFormationProImpossibiliteTravail: false,
            obligationScolaire: {
              kind: "Pendant",
              payload: null,
            },
            situationGardeAlternee: {
              kind: "PasDeGardeAlternee",
              payload: null,
            },
            dateDeNaissance: "2015-01-01",
            identifiant: 0,
          },
        },
        {
          kind: "EnfantACharge",
          payload: {
            aDejaOuvertDroitAuxAllocationsFamiliales: true,
            remunerationMensuelle: 0,
            nationalite: {
              kind: "Francaise",
              payload: null,
            },
            etudesApprentissageStageFormationProImpossibiliteTravail: false,
            obligationScolaire: {
              kind: "Pendant",
              payload: null,
            },
            situationGardeAlternee: {
              kind: "PasDeGardeAlternee",
              payload: null,
            },
            dateDeNaissance: "2016-01-01",
            identifiant: 1,
          },
        },
      ],
      nombreAutresOccupantsLogement: 0,
      situationFamiliale: {
        kind: "Concubins",
        payload: null,
      },
      conditionRattacheFoyerFiscalParentIfi: false,
      enfantANaitreApresQuatriemeMoisGrossesse: false,
      personnesAgeesHandicapeesFoyerR8444: false,
      residence: {
        kind: "Metropole",
        payload: null,
      },
    },
    demandeurIn: {
      nationalite: {
        kind: "Francaise",
        payload: null,
      },
      estNonSalarieAgricoleL7818L78146CodeRural: false,
      magistratFonctionnaireCentreInteretsMaterielsFamiliauxHorsMayotte: false,
      patrimoine: {
        produisantRevenuPeriodeR82233R8224: 0,
        neProduisantPasRevenuPeriodeR82233R8224: 0,
      },
      personneHebergeeCentreSoinLL162223SecuriteSociale: false,
      dateNaissance: "1992-01-01",
    },
    dateCouranteIn: "2022-05-01",
    ressourcesMenagePrisesEnCompteIn: 11500,
  });
  if (log) {
    console.log(
      util.inspect(Law.eventsManager.retrieveEvents(0).slice(1, 10), {
        showHidden: false,
        depth: null,
        colors: true,
      }),
    );
  }
  Law.eventsManager.resetLog();
  console.log("Housing benefits mounthly amount:", result, "€");
}

try {
  run_computation_AF(false);
  run_computation_AL(false);
} catch (error) {
  console.error(error.message);
  process.exit(1);
}
process.exit(0);

// suite
//   .add("AllocationFamiliales#benchmark", function () {
//     run_computation_AF(false);
//   })
//   .on("cycle", function (event) {
//     console.log(String(event.target));
//   })
//   .run({ async: true });
