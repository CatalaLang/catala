from .random_input_generator import generate_random_input
from .input import LogementFoyer, CnafSimulatorInput, Enfant, SeulOuCouple, Zone
from .call_cnaf import run_simulator
from .cnaf_to_catala import run_catala_by_converting_cnaf_input
from catala.runtime import LogEventCode, retrieve_log
from termcolor import colored


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
# sample_input = CnafSimulatorInput(
#     zone=Zone.Zone2,
#     logement=LogementCrous(typ=LogementCrousType.Chambre_rehabilitee),
#     loyer=400,
#     seul_ou_couple=SeulOuCouple.Seul,
#     enfants=[],
#     revenu_pris_en_compte=6_000
# )
# sample_input = CnafSimulatorInput(
#     zone=Zone.Zone1,
#     logement=LogementFoyer(),
#     loyer=804,
#     seul_ou_couple=SeulOuCouple.Seul,
#     enfants=[
#         Enfant(age=14),
#         Enfant(age=13),
#         Enfant(age=14),
#         Enfant(age=12),
#         Enfant(age=22),
#     ],
#     revenu_pris_en_compte=1_600
# )


print_log = False
# input identical to the JS test of the housing benefits
sample_input = CnafSimulatorInput(
    zone=Zone.Zone1,
    logement=LogementFoyer(),
    loyer=804,
    seul_ou_couple=SeulOuCouple.Seul,
    enfants=[
        Enfant(age=14),
        Enfant(age=13),
        Enfant(age=14),
        Enfant(age=12),
        Enfant(age=22),
    ],
    revenu_pris_en_compte=1_600
)
# Or a random input
# sample_input = generate_random_input()
print("🏡 Description du ménage")
print(sample_input)
housing_benefits_catala = run_catala_by_converting_cnaf_input(sample_input)
print("💰 Aides au logement (Catala): {} €".format(housing_benefits_catala))
housing_benefits_cnaf = run_simulator(sample_input)
print("💰 Aides au logement (CNAF) : {} €".format(housing_benefits_cnaf))

if print_log:
    log = retrieve_log()
    indentation = 0
    for log_event in log:
        if log_event.code == LogEventCode.BeginCall:
            print("{}{} {}".format(
                "".ljust(indentation), colored("Begin call:", "yellow"), colored(" >> ".join(log_event.payload), "magenta")))  # type: ignore
            indentation += 2
        elif log_event.code == LogEventCode.EndCall:
            indentation -= 2
            print("{}{} {}".format(
                "".ljust(indentation), colored("End call:", "yellow"), colored(" >> ".join(log_event.payload), "magenta")))  # type: ignore
        elif log_event.code == LogEventCode.VariableDefinition:
            headings, value = log_event.payload  # type: ignore
            print("{}{} {} {} {}".format(
                "".ljust(indentation), colored("Variable definition:", "blue"), colored(" >> ".join(headings), "magenta"), colored(":=", "blue"), colored(print_value(value), "green")))  # type: ignore
        elif log_event.code == LogEventCode.DecisionTaken:
            print("{}{} {}".format(
                "".ljust(indentation), colored("Decision taken:", "green"), colored("{}".format(log_event.payload), "magenta")))  # type: ignore

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
