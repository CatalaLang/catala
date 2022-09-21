#!python3

from datetime import date
from tkinter import N
from src.aides_logement import ModeOccupation_Code, Nationalite_Code, PrestationRecue_Code, SituationFamiliale_Code, SituationGardeAlternee_Code, SituationObligationScolaire_Code, TypeBailleur_Code, ZoneDHabitation_Code
from src.allocations_familiales import PriseEnCharge_Code, Collectivite_Code, SituationObligationScolaire
from src.api import EnfantAPL, InfosLocation, aides_logement, allocations_familiales, Enfant
from catala.runtime import LogEvent, LogEventCode, reset_log, retrieve_log
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
                   a_deja_ouvert_droit_aux_allocations_familiales=True,
                   beneficie_titre_personnel_aide_personnelle_logement=False),
            Enfant(id=1, remuneration_mensuelle=300,
                   date_de_naissance=date(2013, 9, 30),
                   prise_en_charge=PriseEnCharge_Code.GardeAlterneePartageAllocations,
                   a_deja_ouvert_droit_aux_allocations_familiales=True,
                   beneficie_titre_personnel_aide_personnelle_logement=False)
        ],
        ressources_menage=30000,
        residence=Collectivite_Code.Metropole,
        personne_charge_effective_permanente_est_parent=True,
        personne_charge_effective_permanente_remplit_titre_I=True,
        avait_enfant_a_charge_avant_1er_janvier_2012=False,
    )


def call_aides_logement() -> float:
    return aides_logement(
        date_courante=date(2022, 1, 1),
        ressources_menage_prises_en_compte=11_500,
        date_naissance_demandeur=date(1992, 1, 1),
        nationalite_demandeur=Nationalite_Code.Francaise,
        patrimoine_produisant_revenu=0,
        patrimoine_ne_produisant_pas_revenu=0,
        personne_hebergee_centre_soins=False,
        personne_rattache_foyer_fiscal_parent_ifi=False,
        nombre_autres_occupants_logement_hors_menage=0,
        enfant_a_naitre_apres_quatrieme_mois_grossesse=False,
        situation_familiale=SituationFamiliale_Code.Concubins,
        date_mariage=None,
        prestations_recues=[],
        residence_principale=True,
        logement_est_maison_de_retraite=False,
        surface_logement_m_carres=65,
        zone=ZoneDHabitation_Code.Zone1,
        parts_logement_propriete_famille=None,
        parts_logement_usufruits_famille=None,
        date_naissance_et_conformite_sous_locataire_tiers=None,
        mode_occupation=ModeOccupation_Code.Locataire,
        personnes_a_charge=[
            EnfantAPL(
                identifiant=1,
                beneficie_titre_personnel_aide_personnelle_logement=False,
                a_deja_ouvert_droit_aux_allocations_familiales=True,
                date_de_naissance=date(2015, 1, 1),
                remuneration_mensuelle=0,
                obligation_scolaire=SituationObligationScolaire_Code.Pendant,
                situation_garde_alternee=SituationGardeAlternee_Code.PasDeGardeAlternee,
                coefficient_garde_alternee=None
            ),
            EnfantAPL(
                identifiant=2,
                beneficie_titre_personnel_aide_personnelle_logement=False,
                a_deja_ouvert_droit_aux_allocations_familiales=True,
                date_de_naissance=date(2016, 1, 1),
                remuneration_mensuelle=0,
                obligation_scolaire=SituationObligationScolaire_Code.Pendant,
                situation_garde_alternee=SituationGardeAlternee_Code.PasDeGardeAlternee,
                coefficient_garde_alternee=None)
        ],
        logement_est_decent=True,
        infos_specifiques=InfosLocation(
            loyer_principal=450,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            colocation=False,
            logement_est_chambre=False,
            agees_ou_handicap_adultes_hebergees_onereux_particuliers=False,
            logement_meuble_d842_2=False,
            ancien_loyer_et_apl_relogement=None,
            type_bailleur=TypeBailleur_Code.BailleurPrive,
            bailleur_conventionne=None,
            reduction_loyer_solidarite=None
        )

    )


def benchmark_iteration():
    money_given = call_allocations_familiales()
    assert (money_given == 99.46)


def run_with_log() -> List[LogEvent]:
    money_given = call_allocations_familiales()
    assert (money_given == 99.46)
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
        iterations = 1000
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
        print(call_aides_logement())
        exit(-1)
