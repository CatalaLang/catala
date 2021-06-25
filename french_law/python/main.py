#!python3

from src.catala import date_of_numbers, Unit, integer_of_int, money_of_units_int, no_input, money_to_float
from src.allocations_familiales import interface_allocations_familiales, InterfaceAllocationsFamilialesIn, EnfantEntree, PriseEnCharge, PriseEnCharge_Code, Collectivite, Collectivite_Code
import timeit


def iteration():
    out = interface_allocations_familiales(
        InterfaceAllocationsFamilialesIn(
            date_courante_in=lambda _: date_of_numbers(2020, 4, 20),
            enfants_in=lambda _:  [
                EnfantEntree(d_identifiant=integer_of_int(0), d_remuneration_mensuelle=money_of_units_int(0),
                             d_date_de_naissance=date_of_numbers(2003, 2, 2),
                             d_prise_en_charge=PriseEnCharge(
                                 PriseEnCharge_Code.EffectiveEtPermanente, Unit()),
                             d_a_deja_ouvert_droit_aux_allocations_familiales=True),
                EnfantEntree(d_identifiant=integer_of_int(1), d_remuneration_mensuelle=money_of_units_int(300),
                             d_date_de_naissance=date_of_numbers(2013, 9, 30),
                             d_prise_en_charge=PriseEnCharge(
                                 PriseEnCharge_Code.GardeAlterneePartageAllocations, Unit()),
                             d_a_deja_ouvert_droit_aux_allocations_familiales=True)
            ],
            ressources_menage_in=lambda _: money_of_units_int(30000),
            residence_in=lambda _: Collectivite(
                Collectivite_Code.Metropole, Unit()),
            personne_charge_effective_permanente_est_parent_in=lambda _: True,
            personne_charge_effective_permanente_remplit_titre_I_in=lambda _: True,
            enfants_a_charge_in=no_input(),
            montant_verse_in=no_input()
        ))
    money_given = money_to_float(out.montant_verse_out)
    assert (money_given == 99.37)


iterations = 10000

if __name__ == '__main__':
    print("Iterating {} iterations of the family benefits computation. Total time (s):".format(iterations))
    print(timeit.timeit(iteration, number=iterations))
