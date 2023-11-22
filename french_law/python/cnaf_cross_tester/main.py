from .random_input_generator import generate_random_input
from .input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, Enfant, SeulOuCouple, Zone
from .call_cnaf import run_simulator
from .cnaf_to_catala import run_catala_by_converting_cnaf_input

# input identical to the JS test of the housing benefits
sample_input = CnafSimulatorInput(
    zone=Zone.Zone1,
    logement=AppartementOuMaison(
        AppartementOuMaisonType.Colocation, meuble=False),
    loyer=1600,
    seul_ou_couple=SeulOuCouple.EnCouple,
    enfants=[Enfant(age=7, remuneration_derniere_annee=0),
             Enfant(age=8, remuneration_derniere_annee=0)],
    revenu_pris_en_compte=11_500
)
# Or a random input
# sample_input = generate_random_input()
print("🏡 Description du ménage")
print(sample_input)
housing_benefits_catala = run_catala_by_converting_cnaf_input(sample_input)
print("💰 Aides au logement (Catala): {} €".format(housing_benefits_catala))
housing_benefits_cnaf = run_simulator(sample_input)
print("💰 Aides au logement (CNAF) : {} €".format(housing_benefits_cnaf))
delta = abs(housing_benefits_catala - housing_benefits_cnaf)
if delta == 0:
    print("✅ Pas de difference")
    exit(0)
else:
    print("❌ Différence")
    exit(-1)
