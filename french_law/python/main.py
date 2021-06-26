#!python3

from datetime import date
from src.allocations_familiales import PriseEnCharge_Code, Collectivite_Code
from src.api import allocations_familiales, Enfant
import timeit
import argparse


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
    )


def benchmark_iteration():
    money_given = call_allocations_familiales()
    assert (money_given == 99.37)


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
    else:
        print("Action '{}' not recognized!".format(action))
        exit(-1)
