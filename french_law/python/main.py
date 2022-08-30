#!python3

from datetime import date
from src.allocations_familiales import PriseEnCharge_Code, Collectivite_Code
from src.api import allocations_familiales, Enfant
from src.catala import LogEvent, LogEventCode, reset_log, retrieve_log
import timeit
import argparse
from typing import List, Any
from termcolor import colored


def call_allocations_familiales() -> float:
    return allocations_familiales(
        date_courante=date(2020, 4, 20),
        enfants=[
            Enfant(id=0, remuneration_mensuelle=0,
                   date_de_naissance=date(2003, 2, 2),
                   prise_en_charge=PriseEnCharge_Code.EffectiveEtPermanente,
                   a_deja_ouvert_droit_aux_allocations_familiales=True),
            Enfant(id=1, remuneration_mensuelle=300,
                   date_de_naissance=date(2013, 9, 30),
                   prise_en_charge=PriseEnCharge_Code.GardeAlterneePartageAllocations,
                   a_deja_ouvert_droit_aux_allocations_familiales=True)
        ],
        ressources_menage=30000,
        residence=Collectivite_Code.Metropole,
        personne_charge_effective_permanente_est_parent=True,
        personne_charge_effective_permanente_remplit_titre_I=True,
        avait_enfant_a_charge_avant_1er_janvier_2012=False,
    )


def benchmark_iteration():
    money_given = call_allocations_familiales()
    assert (money_given == 99.37)


def run_with_log() -> List[LogEvent]:
    money_given = call_allocations_familiales()
    assert (money_given == 99.37)
    log = retrieve_log()
    reset_log()
    return log


def print_value(v: Any) -> str:
    if isinstance(v, list):
        return "[" + ",".join([str(x) for x in v]) + "]"
    else:
        return str(v)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='French law library in Python')
    parser.add_argument('action', metavar='ACTION', type=str, nargs=1,
                        help="'bench' or 'show_log'")

    args = parser.parse_args()
    action = args.action[0]
    if action == "bench":
        iterations = 10000
        print("Iterating {} iterations of the family benefits computation. Total time (s):".format(
            iterations))
        print(timeit.timeit(benchmark_iteration, number=iterations))
    elif action == "show_log":
        log = run_with_log()
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
    else:
        print("Action '{}' not recognized!".format(action))
        exit(-1)
