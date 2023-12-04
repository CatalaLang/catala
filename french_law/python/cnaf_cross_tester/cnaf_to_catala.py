import datetime
from typing import List
from catala.runtime import Unit  # type: ignore
from ..src.aides_logement import Collectivite, Collectivite_Code, Nationalite, CategorieEquivalenceLoyerAllocationLogementFoyer_Code, LogementFoyer, ModeOccupation_Code, Nationalite_Code, SituationFamiliale_Code, SituationGardeAlternee_Code, SituationObligationScolaire_Code, TypeBailleur_Code, TypeLogementFoyer_Code, ZoneDHabitation_Code
from ..src.api import EnfantAPL, InfosLocation, InfosLogementFoyer, InfosSpecifiques, PersonneAChargeAPL, aides_logement
from .input import Logement_Code, AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, LogementCrous, LogementCrousType, LogementMaisonRetraite, LogementResidenceSocialeFJT, SeulOuCouple, Zone


def run_catala_by_converting_cnaf_input(sample_input: CnafSimulatorInput) -> float:
    enfants: List[PersonneAChargeAPL] = []
    i = 0
    for enfant in sample_input.enfants:
        enfants.append(EnfantAPL(
            identifiant=i,
            a_deja_ouvert_droit_aux_allocations_familiales=False,
            date_de_naissance=datetime.date.today() - datetime.timedelta(days=366 * enfant.age),
            remuneration_mensuelle=int(0),
            obligation_scolaire=SituationObligationScolaire_Code.Avant if enfant.age < 3 else (
                SituationObligationScolaire_Code.Apres if enfant.age > 16 else SituationObligationScolaire_Code.Pendant),
            situation_garde_alternee=SituationGardeAlternee_Code.PasDeGardeAlternee,
            coefficient_garde_alternee=None,
            nationalite=Nationalite(
                code=Nationalite_Code.Francaise, value=Unit()),
            etudes_apprentissage_stage_formation_pro_impossibilite_travail=False
        ))
        i += 1

    mode_occupation: ModeOccupation_Code
    infos_specifiques: InfosSpecifiques
    if sample_input.logement.code() == Logement_Code.CodeAppartementOuMaison:
        mode_occupation = ModeOccupation_Code.Locataire
        infos_specifiques = InfosLocation(
            loyer_principal=sample_input.loyer,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=sample_input.logement.typ_v == AppartementOuMaisonType.Colocation,
            agees_ou_handicap_adultes_hebergees_onereux_particuliers=False,
            logement_meuble_d842_2=sample_input.logement.meuble_v,
            ancien_loyer_et_apl_relogement=None,
            type_bailleur=TypeBailleur_Code.BailleurPrive,
            bailleur_conventionne=None,
            reduction_loyer_solidarite=None
        )
    elif sample_input.logement.code() == Logement_Code.CodeLogementCrous:
        # Les correspondances avec les catégories réglementaires sont faites selon DGALN/DHUP/FE4 (mail du 26/07/2022)
        mode_occupation = ModeOccupation_Code.Locataire if sample_input.logement.typ_v == LogementCrousType.Studio else ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLocation(
            loyer_principal=sample_input.loyer,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=False,
            logement_meuble_d842_2=False,
            ancien_loyer_et_apl_relogement=None,
            type_bailleur=TypeBailleur_Code.BailleurPrive,
            agees_ou_handicap_adultes_hebergees_onereux_particuliers=False,
            bailleur_conventionne=None,
            reduction_loyer_solidarite=None,
        ) if sample_input.logement.typ_v == LogementCrousType.Studio else InfosLogementFoyer(
            type=TypeLogementFoyer_Code.Autre,
            remplit_conditions_r832_21=True,
            conventionne_livre_III_titre_V_chap_III=True,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.EtudiantLogeEnChambreCROUS if
            sample_input.logement.typ_v == LogementCrousType.Chambre else
            CategorieEquivalenceLoyerAllocationLogementFoyer_Code.EtudiantLogeEnChambreCROUSRehabilitee,
            conventionne_selon_regles_drom=False,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=False,
            logement_meuble_d842_2=False,
            logement_foyer_jeunes_travailleurs=False
        )
    elif sample_input.logement.code() == Logement_Code.CodeLogementFoyer:
        mode_occupation = ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLogementFoyer(
            conventionne_selon_regles_drom=False,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=False,
            logement_meuble_d842_2=False,
            logement_foyer_jeunes_travailleurs=False,
            type=TypeLogementFoyer_Code.Autre,
            remplit_conditions_r832_21=False,
            conventionne_livre_III_titre_V_chap_III=False,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.AutresPersonnes
        )
    elif sample_input.logement.code() == Logement_Code.CodeLogementResidenceSocialeFJT:
        # Correspond au 2° du D832-25 selon DGALN/DHUP/FE4 (mail du 26/07/2022)
        mode_occupation = ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLogementFoyer(
            conventionne_selon_regles_drom=False,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=False,
            logement_meuble_d842_2=False,
            logement_foyer_jeunes_travailleurs=False,
            type=TypeLogementFoyer_Code.ResidenceSociale,
            remplit_conditions_r832_21=True,
            conventionne_livre_III_titre_V_chap_III=True,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.AutresPersonnes
        )
    elif sample_input.logement.code() == Logement_Code.CodeLogementMaisonRetraite:
        # Correspond au 3° du D842-16 et au 1° du R832-20 selon DGALN/DHUP/FE4 (mail du 26/07/2022)
        mode_occupation = ModeOccupation_Code.ResidentLogementFoyer
        infos_specifiques = InfosLogementFoyer(
            conventionne_selon_regles_drom=False,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=False,
            colocation=False,
            logement_meuble_d842_2=False,
            logement_foyer_jeunes_travailleurs=False,
            type=TypeLogementFoyer_Code.LogementPersonnesAgeesOuHandicapees if sample_input.logement.conventionne else TypeLogementFoyer_Code.Autre,
            remplit_conditions_r832_21=True,
            conventionne_livre_III_titre_V_chap_III=sample_input.logement.conventionne,
            date_conventionnement=datetime.date(2000, 1, 1),
            construit_application_loi_1957_12_III=False,
            redevance=sample_input.loyer,
            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer_Code.PersonnesAgeesSelon3DeD842_16
        )
    else:  # sample_input.logement.code() == Logement_Code.CodeLogementChambre:
        mode_occupation = ModeOccupation_Code.Locataire
        infos_specifiques = InfosLocation(
            loyer_principal=sample_input.loyer,
            beneficiaire_aide_adulte_ou_enfant_handicapes=False,
            logement_est_chambre=True,
            colocation=False,
            agees_ou_handicap_adultes_hebergees_onereux_particuliers=False,
            logement_meuble_d842_2=sample_input.logement.meuble_v,
            ancien_loyer_et_apl_relogement=None,
            type_bailleur=TypeBailleur_Code.BailleurPrive,
            bailleur_conventionne=None,
            reduction_loyer_solidarite=None
        )

    housing_benefits_gross = aides_logement(
        datetime.date.today(),
        ressources_menage_prises_en_compte=sample_input.revenu_pris_en_compte,
        date_naissance_demandeur=datetime.date(1992, 1, 1),
        nationalite_demandeur=Nationalite(
            code=Nationalite_Code.Francaise, value=Unit()),
        personne_hebergee_centre_soins=False,
        personnes_agees_handicapees_foyer_r844_4=False,
        magistrat_fonctionnaire_centre_interets_materiels_familiaux_hors_mayotte=False,
        est_non_salarie_agricole_l781_8_l_781_46_code_rural=False,
        residence=Collectivite_Code.Metropole,
        personne_rattache_foyer_fiscal_parent_ifi=False,
        nombre_autres_occupants_logement_hors_menage=0,
        enfant_a_naitre_apres_quatrieme_mois_grossesse=False,
        situation_familiale=SituationFamiliale_Code.Celibataire if sample_input.seul_ou_couple == SeulOuCouple.Seul else SituationFamiliale_Code.Concubins,
        date_mariage=None,
        prestations_recues=[],
        residence_principale=True,
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
