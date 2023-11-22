from .random_input_generator import generate_random_input
from .input import LogementCrous, LogementCrousType, CnafSimulatorInput, Enfant, SeulOuCouple, Zone
from .call_cnaf import run_simulator
from .cnaf_to_catala import run_catala_by_converting_cnaf_input


def print_value(v) -> str:
    if isinstance(v, list):
        return "[" + ",".join([str(x) for x in v]) + "]"
    else:
        return str(v)

# Cas à débugguer :
# sample_input = CnafSimulatorInput(
#     zone=Zone.Zone2,
#     logement=LogementResidenceSocialeFJT(),
#     loyer=200,
#     seul_ou_couple=SeulOuCouple.Seul,
#     enfants=[],
#     revenu_pris_en_compte=9_000
# )

# input identical to the JS test of the housing benefits
sample_input = CnafSimulatorInput(
    zone=Zone.Zone2,
    logement=LogementCrous(typ=LogementCrousType.Chambre_rehabilitee),
    loyer=400,
    seul_ou_couple=SeulOuCouple.Seul,
    enfants=[],
    revenu_pris_en_compte=6_000
)
# Or a random input
# sample_input = generate_random_input()
print("🏡 Description du ménage")
housing_benefits_catala = run_catala_by_converting_cnaf_input(sample_input)
print("💰 Aides au logement (Catala): {} €".format(housing_benefits_catala))
housing_benefits_cnaf = run_simulator(sample_input)
print("💰 Aides au logement (CNAF) : {} €".format(housing_benefits_cnaf))
delta = abs(housing_benefits_catala - housing_benefits_cnaf)
if delta == 0:
    print("✅ Pas de difference")
    exit(0)
elif delta == 1:
    print("⚠️  Différence de +/- 1 €, on laisse tomber les erreurs d'arrondi...")
    exit(0)
else:
    print("❌ Différence")
    exit(-1)
