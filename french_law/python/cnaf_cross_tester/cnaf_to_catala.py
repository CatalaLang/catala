import datetime
from typing import List
from src.aides_logement import CategorieEquivalenceLoyerAllocationLogementFoyer_Code, LogementFoyer, ModeOccupation_Code, Nationalite_Code, SituationFamiliale_Code, SituationGardeAlternee_Code, SituationObligationScolaire_Code, TypeBailleur_Code, TypeLogementFoyer_Code, ZoneDHabitation_Code
from input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, Enfant, LogementCrous, LogementCrousType, LogementMaisonRetraite, LogementResidenceSocialeFJT, SeulOuCouple, Zone
from src.api import EnfantAPL, InfosLocation, InfosLogementFoyer, InfosSpecifiques, PersonneAChargeAPL, aides_logement


def run_catala_by_converting_cnaf_input(sample_input: CnafSimulatorInput) -> float:
    enfants: List[PersonneAChargeAPL] = []
    i = 0
    for enfant in sample_input.enfants:
        enfants.append(EnfantAPL(
            identifiant=i,
            beneficie_titre_personnel_aide_personnelle_logement=False,
            a_deja_ouvert_droit_aux_allocations_familiales=False,
            date_de_naissance=datetime.date.today() - datetime.timedelta(days=366 * enfant.age),
            remuneration_mensuelle=int(
                enfant.remuneration_derniere_annee / 12),
            obligation_scolaire=SituationObligationScolaire_Code.Avant if enfant.age < 3 else (
                SituationObligationScolaire_Code.Apres if enfant.age > 16 else SituationObligationScolaire_Code.Pendant),
            situation_garde_alternee=SituationGardeAlternee_Code.PasDeGardeAlternee,
            coefficient_garde_alternee=None
        ))
        i += 1

    infos_specifiques: InfosSpecifiques
    if isinstance(sample_input.logement, AppartementOuMaison):
        mode_occupation = (ModeOccupation_Code.Locataire if sample_input.logement.typ(
        ) == AppartementOuMaisonType.Location else ModeOccupation_Code.SousLocataire)
        infos_specifiques = InfosLocation(
            loyer_principal=sample_input.loyer,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=False,
            agees_ou_handicap_adultes_hebergees_onereux_particuliers=False,
            logement_meuble_d842_2=sample_input.logement.meuble_v,
            ancien_loyer_et_apl_relogement=None,
            type_bailleur=TypeBailleur_Code.BailleurPrive,
            bailleur_conventionne=None,
            reduction_loyer_solidarite=None
        )
    elif isinstance(sample_input.logement, LogementCrous):
        # Les correspondances avec les catégories réglementaires sont faites selon DGALN/DHUP/FE4 (mail du 26/07/2022)
        mode_occupation = ModeOccupation_Code.Locataire if sample_input.logement.typ_v == LogementCrousType.Studio else ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLocation(
            loyer_principal=sample_input.loyer,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=False,
            agees_ou_handicap_adultes_hebergees_onereux_particuliers=False,
            logement_meuble_d842_2=False,
            ancien_loyer_et_apl_relogement=None,
            type_bailleur=TypeBailleur_Code.BailleurPrive,
            bailleur_conventionne=None,
            reduction_loyer_solidarite=None
        ) if sample_input.logement.typ_v == LogementCrousType.Studio else InfosLogementFoyer(
            type=TypeLogementFoyer_Code.Autre,
            remplit_conditions_r832_21=True,
            conventionne_livre_III_titre_V_chap_III=True,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.EtudiantLogeEnChambreCROUS if
            sample_input.logement.typ_v == LogementCrousType.Chambre else
            CategorieEquivalenceLoyerAllocationLogementFoyer_Code.EtudiantLogeEnChambreCROUSRehabilitee
        )
    elif isinstance(sample_input.logement, LogementFoyer):
        mode_occupation = ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLogementFoyer(
            type=TypeLogementFoyer_Code.Autre,
            remplit_conditions_r832_21=True,
            conventionne_livre_III_titre_V_chap_III=True,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.AutresPersonnes
        )
    elif isinstance(sample_input.logement, LogementResidenceSocialeFJT):
        # Correspond au 2° du D832-25 selon DGALN/DHUP/FE4 (mail du 26/07/2022)
        mode_occupation = ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLogementFoyer(
            type=TypeLogementFoyer_Code.ResidenceSociale,
            remplit_conditions_r832_21=True,
            conventionne_livre_III_titre_V_chap_III=True,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.AutresPersonnes
        )
    elif isinstance(sample_input.logement, LogementMaisonRetraite):
        # Correspond au 3° du D842-16 et au 1° du R832-20 selon DGALN/DHUP/FE4 (mail du 26/07/2022)
        mode_occupation = ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLogementFoyer(
            type=TypeLogementFoyer_Code.LogementPersonnesAgeesOuHandicapees,
            remplit_conditions_r832_21=True,
            conventionne_livre_III_titre_V_chap_III=True,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.PersonnesAgeesSelon3DeD842_16
        )
    else:  # isinstance(sample_input.logement, LogementChambre):
        mode_occupation = ModeOccupation_Code.Locataire
        infos_specifiques = InfosLocation(
            loyer_principal=sample_input.loyer,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=True,
            colocation=False,
            agees_ou_handicap_adultes_hebergees_onereux_particuliers=False,
            logement_meuble_d842_2=False,
            ancien_loyer_et_apl_relogement=None,
            type_bailleur=TypeBailleur_Code.BailleurPrive,
            bailleur_conventionne=None,
            reduction_loyer_solidarite=None
        )

    housing_benefits_gross = aides_logement(
        datetime.date.today(),
        ressources_menage_prises_en_compte=sample_input.revenu_pris_en_compte,
        date_naissance_demandeur=datetime.date(1992, 1, 1),
        nationalite_demandeur=Nationalite_Code.Francaise,
        patrimoine_produisant_revenu=0,
        patrimoine_ne_produisant_pas_revenu=0,
        personne_hebergee_centre_soins=False,
        personne_rattache_foyer_fiscal_parent_ifi=False,
        nombre_autres_occupants_logement_hors_menage=0,
        enfant_a_naitre_apres_quatrieme_mois_grossesse=False,
        situation_familiale=SituationFamiliale_Code.Celibataire if sample_input.seul_ou_couple == SeulOuCouple.Seul else SituationFamiliale_Code.Concubins,
        date_mariage=None,
        prestations_recues=[],
        residence_principale=True,
        logement_est_maison_de_retraite=True if isinstance(
            sample_input.logement, LogementMaisonRetraite) else False,
        logement_est_decent=True,
        surface_logement_m_carres=10000,
        zone=ZoneDHabitation_Code.Zone1 if sample_input.zone == Zone.Zone1 else (
            ZoneDHabitation_Code.Zone2 if sample_input.zone == Zone.Zone2 else ZoneDHabitation_Code.Zone3),
        parts_logement_propriete_famille=None,
        parts_logement_usufruits_famille=None,
        date_naissance_et_conformite_sous_locataire_tiers=None,
        personnes_a_charge=enfants,
        mode_occupation=mode_occupation,
        infos_specifiques=infos_specifiques)

    return round(housing_benefits_gross*0.995)  # We take the CRDS
