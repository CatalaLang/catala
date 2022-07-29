var Law = require("./french_law.js");
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
      })
    );
  }
  Law.eventsManager.resetLog();
  console.log("Family benefits mounthly amount:", result, "€");
}

function run_computation_AL(log) {
  var result = Law.computeAidesAuLogement({
    dateCouranteIn: "2022-01-01",
    menageIn: {
      prestationsRecues: [
        { kind: "AllocationSoutienEnfantHandicape", payload: null },
        { kind: "ComplementFamilial", payload: null },
        { kind: "AllocationsFamiliales", payload: null },
      ],
      situationFamiliale: {
        kind: "Maries",
        payload: "2010-11-26",
      },
      personnesACharge: [
        {
          kind: "EnfantACharge",
          payload: {
            beneficieTitrePersonnelAidePersonnelleLogement: false,
            priseEnCharge: { kind: "EffectiveEtPermanente", payload: null },
            age: 19,
            identifiant: 0,
            aDejaOuvertDroitAuxAllocationsFamiliales: true,
            dateDeNaissance: "2003-01-01",
            remunerationMensuelle: 0,
            obligationScolaire: { kind: "Apres", payload: null },
            situationGardeAlternee: {
              kind: "GardeAlterneeCoefficientPriseEnCharge",
              payload: 0.5,
            },
          },
        },
        {
          kind: "EnfantACharge",
          payload: {
            beneficieTitrePersonnelAidePersonnelleLogement: false,
            priseEnCharge: { kind: "EffectiveEtPermanente", payload: null },
            age: 11,
            identifiant: 1,
            aDejaOuvertDroitAuxAllocationsFamiliales: true,
            dateDeNaissance: "2011-01-01",
            remunerationMensuelle: 0,
            obligationScolaire: { kind: "Pendant", payload: null },
            situationGardeAlternee: {
              kind: "PasDeGardeAlternee",
              payload: null,
            },
          },
        },
        {
          kind: "EnfantACharge",
          payload: {
            beneficieTitrePersonnelAidePersonnelleLogement: false,
            priseEnCharge: { kind: "EffectiveEtPermanente", payload: null },
            age: 8,
            identifiant: 2,
            aDejaOuvertDroitAuxAllocationsFamiliales: true,
            dateDeNaissance: "2014-01-01",
            remunerationMensuelle: 0,
            obligationScolaire: { kind: "Pendant", payload: null },
            situationGardeAlternee: {
              kind: "PasDeGardeAlternee",
              payload: null,
            },
          },
        },
      ],
      logement: {
        zone: { kind: "Zone1", payload: null },
        residencePrincipale: true,
        estEhpadOuMaisonAutonomieL31312Asf: false,
        modeOccupation: {
          kind: "Locataire",
          payload: {
            bailleur: {
              typeBailleur: { kind: "BailleurPrive", payload: null },
              respecteConventionTitreV: true,
              respecteConventionTitreII: true,
              construitAmelioreConditionsL83114: false,
              acquisitionAidesEtatPretTitreIIOuLivreIII: false,
            },
          },
        },
        proprietaire: { kind: "Autre", payload: null },
        loueOuSousLoueADesTiers: { kind: "Non", payload: null },
        usufruit: { kind: "Autre", payload: null },
        logementDecentL89462: true,
        loyersL8233: 700,
        surfaceMCarres: 80,
        estAncienL8312: false,
        situeCommuneDesequilibreL8312: false,
      },
      nombreAutresOccupantsLogement: 1,
      conditionRattacheFoyerFiscalParentIfi: false,
      nombreEnfantsANaitreApresTroisiemeMoisGrossesse: 0,
      enfantANaitreApresQuatriemeMoisGrossesse: false,
      dateNaissanceTroisiemeEnfantOuDernierSiPlus: {
        kind: "PlusDeTroisEnfants",
        payload: {
          kind: "DateDeNaissance",
          payload: "2014-09-15",
        },
      },
    },
    demandeurIn: {
      personneHebergeeCentreSoinLL162223SecuriteSociale: false,
      satisfaitConditionsL5122CodeSecuriteSociale: true,
      ageDemandeur: 52,
      dateNaissance: "1970-05-02",
      contratDeTravail: { kind: "CDI", payload: null },
      nationalite: { kind: "Francaise", payload: null },
      patrimoine: {
        produisantRevenuPeriodeR82233R8224: 0,
        neProduisantPasRevenuPeriodeR82233R8224: 0,
      },
    },
    informationsCalculIn: {
      kind: "InfosLocatif",
      payload: {
        loyerPrincipal: 1700,
        beneficiaireAideAdulteOuEnfantHandicapes: false,
        logementEstChambre: false,
        colocation: false,
        ageesOuHandicapAdultesHebergeesOnereuxParticuliers: false,
        reductionLoyerSolidarite: 0,
        logementMeubleD8422: false,
        changementLogementD8424: {
          kind: "PasDeChangement",
          payload: null,
        },
      },
    },
    ressourcesMenagePrisesEnCompteIn: 20000,
  });
  if (log) {
    console.log(
      util.inspect(Law.eventsManager.retrieveEvents(0).slice(1, 10), {
        showHidden: false,
        depth: null,
        colors: true,
      })
    );
  }
  Law.eventsManager.resetLog();
  console.log("Housing benefits mounthly amount:", result, "€");
}

try {
  run_computation_AF(false);
  run_computation_AL(false);
} catch (error) {
  console.log(error.message);
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
