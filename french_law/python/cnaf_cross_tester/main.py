

from input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, Enfant, SeulOuCouple
from pupeteer import run_simulator

# Output identical to the JS test of the housing benefits
sample_input = CnafSimulatorInput(
    code_postal="69001",
    logement=AppartementOuMaison(
        AppartementOuMaisonType.Location, meuble=False),
    loyer=450,
    seul_ou_couple=SeulOuCouple.EnCouple,
    enfants=[Enfant(age=7, remuneration_derniere_annee=0),
             Enfant(age=8, remuneration_derniere_annee=0)],
    revenu_pris_en_compte=11_500
)
print(sample_input)
housing_benefits = run_simulator(sample_input)
print("Aides au logement : {} €".format(housing_benefits))
