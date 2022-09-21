from abc import ABC
from catala.runtime import *
from .allocations_familiales import Collectivite, Collectivite_Code, InterfaceAllocationsFamilialesIn, PriseEnCharge, interface_allocations_familiales, PriseEnCharge_Code, EnfantEntree, InterfaceAllocationsFamilialesIn
from .aides_logement import AutrePersonneACharge, CategorieEquivalenceLoyerAllocationLogementFoyer, CategorieEquivalenceLoyerAllocationLogementFoyer_Code, ChangementLogementD8424, ChangementLogementD8424_Code, ConventionANHA, ConventionBailleurSocial, EnfantACharge, InfosChangementLogementD8424, Location, Logement, LogementFoyer, LoueOuSousLoueADesTiers, LoueOuSousLoueADesTiers_Code, Menage, ModeOccupation, ModeOccupation_Code, Nationalite, Nationalite_Code, NeufOuAncien, NeufOuAncien_Code, ParentOuAutre, ParentOuAutre_Code, Parente, Parente_Code, Patrimoine, PersonneACharge, PersonneSousLocation, PrestationRecue, PrestationRecue_Code, Pret, Proprietaire, SituationFamiliale, SituationFamiliale_Code, SituationGardeAlternee_Code, SituationObligationScolaire_Code, TitulairePret, TitulairePret_Code, TypeBailleur, TypeBailleur_Code, TypeLogementFoyer, TypeLogementFoyer_Code, TypePret, TypePret_Code, TypeTravauxLogementD83215, TypeTravauxLogementD83215_Code, TypeTravauxLogementR8425, TypeTravauxLogementR8425_Code, ZoneDHabitation, ZoneDHabitation_Code, calculette_aides_au_logement_garde_alternee, CalculetteAidesAuLogementGardeAlterneeIn, ressources_aides_personnelle_logement, Demandeur, PersonneACharge_Code, SituationObligationScolaire, SituationGardeAlternee

# Allocations familiales


class Enfant:
    def __init__(
        self,
        id: int,
        remuneration_mensuelle: int,
        date_de_naissance: datetime.date,
        prise_en_charge: PriseEnCharge_Code,
        a_deja_ouvert_droit_aux_allocations_familiales: bool,
        beneficie_titre_personnel_aide_personnelle_logement: bool
    ) -> None:
        self.id = id
        self.remuneration_mensuelle = remuneration_mensuelle
        self.date_de_naissance = date_de_naissance
        self.prise_en_charge = prise_en_charge
        self.a_deja_ouvert_droit_aux_allocations_familiales = a_deja_ouvert_droit_aux_allocations_familiales
        self.beneficie_titre_personnel_aide_personnelle_logement = beneficie_titre_personnel_aide_personnelle_logement

    def to_allocations_familiales(self) -> EnfantEntree:
        return EnfantEntree(
            d_identifiant=integer_of_int(self.id),
            d_remuneration_mensuelle=money_of_units_int(
                self.remuneration_mensuelle),
            d_a_deja_ouvert_droit_aux_allocations_familiales=self.a_deja_ouvert_droit_aux_allocations_familiales,
            d_beneficie_titre_personnel_aide_personnelle_logement=self.beneficie_titre_personnel_aide_personnelle_logement,
            d_date_de_naissance=date_of_datetime(self.date_de_naissance),
            d_prise_en_charge=PriseEnCharge(self.prise_en_charge, Unit())
        )


def allocations_familiales(
        date_courante: datetime.date,
        enfants: List[Enfant],
        ressources_menage: int,
        residence: Collectivite_Code,
        personne_charge_effective_permanente_est_parent: bool,
        personne_charge_effective_permanente_remplit_titre_I: bool,
        avait_enfant_a_charge_avant_1er_janvier_2012: bool
):
    out = interface_allocations_familiales(InterfaceAllocationsFamilialesIn(
        i_date_courante_in=date_of_datetime(date_courante),
        i_enfants_in=[enfant.to_allocations_familiales()
                      for enfant in enfants],
        i_ressources_menage_in=money_of_units_int(ressources_menage),
        i_residence_in=Collectivite(residence, Unit()),
        i_personne_charge_effective_permanente_est_parent_in=personne_charge_effective_permanente_est_parent,
        i_personne_charge_effective_permanente_remplit_titre_I_in=personne_charge_effective_permanente_remplit_titre_I,
        i_avait_enfant_a_charge_avant_1er_janvier_2012_in=avait_enfant_a_charge_avant_1er_janvier_2012
    ))
    return money_to_float(out.i_montant_verse_out)

# Aides au logement


class PersonneAChargeAPL(ABC):
    pass


class EnfantAPL(PersonneAChargeAPL):
    def __init__(self, identifiant: int, beneficie_titre_personnel_aide_personnelle_logement: bool,
                 a_deja_ouvert_droit_aux_allocations_familiales: bool,
                 date_de_naissance: datetime.date,
                 remuneration_mensuelle: float,
                 obligation_scolaire: SituationObligationScolaire_Code,
                 situation_garde_alternee: SituationGardeAlternee_Code,
                 coefficient_garde_alternee: Optional[float]):
        self.identifiant = identifiant
        self.beneficie_titre_personnel_aide_personnelle_logement = beneficie_titre_personnel_aide_personnelle_logement
        self.a_deja_ouvert_droit_aux_allocations_familiales = a_deja_ouvert_droit_aux_allocations_familiales
        self.date_de_naissance = date_de_naissance,
        self.remuneration_mensuelle = remuneration_mensuelle
        self.obligation_scolaire = obligation_scolaire
        self.situation_garde_alternee = situation_garde_alternee
        self.coefficient_garde_alternee = coefficient_garde_alternee


class ParentAPL(PersonneAChargeAPL):
    def __init__(self, date_naissance: datetime.date,
                 ressources: float, ascendant_descendant_collateral_deuxieme_troisieme_degre: bool,
                 parente: Parente_Code,
                 incapacite_80_pourcent_ou_restriction_emploi: bool,
                 beneficiaire_l161_19_l351_8_l643_3_secu: bool,
                 titulaire_allocation_personne_agee: bool):
        self.date_naissance = date_naissance
        self.ressources = ressources
        self.ascendant_descendant_collateral_deuxieme_troisieme_degre = ascendant_descendant_collateral_deuxieme_troisieme_degre
        self.parente = parente
        self.incapacite_80_pourcent_ou_restriction_emploi = incapacite_80_pourcent_ou_restriction_emploi
        self.beneficiaire_l161_19_l351_8_l643_3_secu = beneficiaire_l161_19_l351_8_l643_3_secu
        self.titulaire_allocation_personne_agee = titulaire_allocation_personne_agee


class InfosSpecifiques(ABC):
    pass


class InfosLocation(InfosSpecifiques):
    def __init__(self,
                 loyer_principal: float,
                 beneficiaire_aide_adulte_ou_enfant_handicapes: bool,
                 logement_est_chambre: bool,
                 colocation: bool,
                 agees_ou_handicap_adultes_hebergees_onereux_particuliers: bool,
                 logement_meuble_d842_2: bool,
                 ancien_loyer_et_apl_relogement: Optional[Tuple[float, float]],
                 type_bailleur: TypeBailleur_Code,
                 bailleur_conventionne: Optional[bool],
                 reduction_loyer_solidarite: Optional[float]):
        self.loyer_principal = loyer_principal
        self.beneficiaire_aide_adulte_ou_enfant_handicapes = beneficiaire_aide_adulte_ou_enfant_handicapes
        self.logement_est_chambre = logement_est_chambre
        self.colocation = colocation
        self.agees_ou_handicap_adultes_hebergees_onereux_particuliers = agees_ou_handicap_adultes_hebergees_onereux_particuliers
        self.logement_meuble_d842_2 = logement_meuble_d842_2
        self.ancien_loyer_et_apl_relogement = ancien_loyer_et_apl_relogement
        self.type_bailleur = type_bailleur
        self.bailleur_conventionne = bailleur_conventionne
        self.reduction_loyer_solidarite = reduction_loyer_solidarite


class InfosLogementFoyer(InfosSpecifiques):
    def __init__(self,
                 type: TypeLogementFoyer_Code,
                 remplit_conditions_r832_21: bool,
                 conventionne_livre_III_titre_V_chap_III: bool,
                 date_conventionnement: datetime.date,
                 construit_application_loi_1957_12_III: bool,
                 redevance: float,
                 categorie_equivalence_loyer_d842_16: CategorieEquivalenceLoyerAllocationLogementFoyer_Code):
        self.type = type
        self.remplit_conditions_r832_21 = remplit_conditions_r832_21
        self.conventionne_livre_III_titre_V_chap_III = conventionne_livre_III_titre_V_chap_III
        self.date_conventionnement = date_conventionnement
        self.construit_application_loi_1957_12_III = construit_application_loi_1957_12_III
        self.redevance = redevance
        self.categorie_equivalence_loyer_d842_16 = categorie_equivalence_loyer_d842_16


class InfosAccessionPropriete(InfosSpecifiques):
    def __init__(self,
                 logement_situe_commune_desequilibre_l831_2: bool,
                 mensualite_principale: float,
                 charges_mensuelles_pret: float,
                 date_entree_logement: datetime.date,
                 local_habite_premiere_fois_beneficiaire: bool,
                 copropriete: bool,
                 situation_r822_11_13_17: bool,
                 type_travaux_logement_d832_15: TypeTravauxLogementD83215_Code,
                 type_travaux_logement_r842_5: TypeTravauxLogementR8425_Code,
                 anciennete_logement: NeufOuAncien_Code,
                 ameliore_par_occupant: Optional[bool],
                 type_pret: TypePret_Code,
                 date_signature_pret: datetime.date,
                 titulaire_pret: TitulairePret_Code):
        self.logement_situe_commune_desequilibre_l831_2 = logement_situe_commune_desequilibre_l831_2
        self.mensualite_principale = mensualite_principale
        self.charges_mensuelles_pret = charges_mensuelles_pret
        self.date_entree_logement = date_entree_logement
        self.local_habite_premiere_fois_beneficiaire = local_habite_premiere_fois_beneficiaire
        self.copropriete = copropriete
        self.situation_r822_11_13_17 = situation_r822_11_13_17
        self.type_travaux_logement_d832_15 = type_travaux_logement_d832_15
        self.type_travaux_logement_r842_5 = type_travaux_logement_r842_5
        self.anciennete_logement = anciennete_logement
        self.ameliore_par_occupant = ameliore_par_occupant
        self.type_pret = type_pret
        self.date_signature_pret = date_signature_pret
        self.titulaire_pret = titulaire_pret


def aides_logement(
    date_courante: datetime.date,
    ressources_menage_prises_en_compte: float,
    date_naissance_demandeur: datetime.date,
    nationalite_demandeur: Nationalite_Code,
    patrimoine_produisant_revenu: float,
    patrimoine_ne_produisant_pas_revenu: float,
    personne_hebergee_centre_soins: bool,
    personne_rattache_foyer_fiscal_parent_ifi: bool,
    nombre_autres_occupants_logement_hors_menage: int,
    enfant_a_naitre_apres_quatrieme_mois_grossesse: bool,
    situation_familiale: SituationFamiliale_Code,
    date_mariage: Optional[datetime.date],
    prestations_recues: List[PrestationRecue_Code],
    residence_principale: bool,
    logement_est_maison_de_retraite: bool,
    logement_est_decent: bool,
    surface_logement_m_carres: int,
    zone: ZoneDHabitation_Code,
    parts_logement_propriete_famille: Optional[float],
    parts_logement_usufruits_famille: Optional[float],
    date_naissance_et_conformite_sous_locataire_tiers: Optional[Tuple[datetime.date, bool]],
    mode_occupation: ModeOccupation_Code,
    infos_specifiques: InfosSpecifiques,
    personnes_a_charge: List[PersonneAChargeAPL],
):
    out = calculette_aides_au_logement_garde_alternee(CalculetteAidesAuLogementGardeAlterneeIn(
        menage_in=Menage(
            prestations_recues=[PrestationRecue(
                code=presta, value=Unit()) for presta in prestations_recues],
            logement=Logement(
                residence_principale=residence_principale,
                est_ehpad_ou_maison_autonomie_l313_12_asf=logement_est_maison_de_retraite,
                mode_occupation=ModeOccupation(
                    code=mode_occupation,
                    value=(Location(
                        loyer_principal=money_of_decimal(
                            decimal_of_float(infos_specifiques.loyer_principal)),
                        beneficiaire_aide_adulte_ou_enfant_handicapes=infos_specifiques.beneficiaire_aide_adulte_ou_enfant_handicapes,
                        logement_est_chambre=infos_specifiques.logement_est_chambre,
                        colocation=infos_specifiques.colocation,
                        agees_ou_handicap_adultes_hebergees_onereux_particuliers=infos_specifiques.agees_ou_handicap_adultes_hebergees_onereux_particuliers,
                        logement_meuble_d842_2=infos_specifiques.logement_meuble_d842_2,
                        changement_logement_d842_4=ChangementLogementD8424(
                            code=ChangementLogementD8424_Code.PasDeChangement if infos_specifiques.ancien_loyer_et_apl_relogement is None else
                            ChangementLogementD8424_Code.Changement,
                            value=Unit() if infos_specifiques.ancien_loyer_et_apl_relogement is None else
                            InfosChangementLogementD8424(ancien_loyer_principal=money_of_decimal(decimal_of_float(infos_specifiques.ancien_loyer_et_apl_relogement[0])),
                                                         ancienne_allocation_logement=money_of_decimal(decimal_of_float(infos_specifiques.ancien_loyer_et_apl_relogement[1])))
                        ),
                        bailleur=TypeBailleur(
                            code=infos_specifiques.type_bailleur,
                            value=Unit() if infos_specifiques.type_bailleur == TypeBailleur_Code.BailleurPrive else (
                                ConventionBailleurSocial(
                                    conventionne_livre_III_titre_V_chap_III=False if infos_specifiques.bailleur_conventionne is None else infos_specifiques.bailleur_conventionne,
                                    reduction_loyer_solidarite_percue=money_of_decimal(decimal_of_float(0.0 if infos_specifiques.reduction_loyer_solidarite is None else infos_specifiques.reduction_loyer_solidarite)))
                            ) if infos_specifiques.type_bailleur == TypeBailleur_Code.BailleurSocial else (
                                ConventionANHA(
                                    conventionne_livre_III_titre_II_chap_I_sec_3=False if infos_specifiques.bailleur_conventionne is None else infos_specifiques.bailleur_conventionne)
                                if infos_specifiques.type_bailleur == TypeBailleur_Code.BailleurPriveAvecConventionnementSocial else
                                None  # type: ignore
                            ))
                    ) if isinstance(infos_specifiques, InfosLocation) else
                        (LogementFoyer(
                            type=TypeLogementFoyer(
                                code=infos_specifiques.type, value=Unit()),
                            remplit_conditions_r832_21=infos_specifiques.remplit_conditions_r832_21,
                            conventionne_livre_III_titre_V_chap_III=infos_specifiques.conventionne_livre_III_titre_V_chap_III,
                            date_conventionnement=date_of_datetime(
                                infos_specifiques.date_conventionnement),
                            construit_application_loi_1957_12_III=infos_specifiques.construit_application_loi_1957_12_III,
                            redevance=money_of_decimal(
                                decimal_of_float(infos_specifiques.redevance)),
                            categorie_equivalence_loyer_d842_16=CategorieEquivalenceLoyerAllocationLogementFoyer(
                                code=infos_specifiques.categorie_equivalence_loyer_d842_16,
                                value=Unit()
                            )
                        ) if isinstance(infos_specifiques, InfosLogementFoyer) else
                            (Proprietaire(
                                logement_situe_commune_desequilibre_l831_2=infos_specifiques.logement_situe_commune_desequilibre_l831_2,
                                mensualite_principale=money_of_decimal(
                                    decimal_of_float(infos_specifiques.mensualite_principale)),
                                charges_mensuelles_pret=money_of_decimal(
                                    decimal_of_float(infos_specifiques.charges_mensuelles_pret)),
                                date_entree_logement=date_of_datetime(
                                    infos_specifiques.date_entree_logement),
                                local_habite_premiere_fois_beneficiaire=infos_specifiques.local_habite_premiere_fois_beneficiaire,
                                copropriete=infos_specifiques.copropriete,
                                situation_r822_11_13_17=infos_specifiques.situation_r822_11_13_17,
                                type_travaux_logement_d832_15=TypeTravauxLogementD83215(
                                    code=infos_specifiques.type_travaux_logement_d832_15, value=Unit()),
                                type_travaux_logement_r842_5=TypeTravauxLogementR8425(
                                    code=infos_specifiques.type_travaux_logement_r842_5,
                                    value=Unit()
                                ),
                                anciennete_logement=NeufOuAncien(code=infos_specifiques.anciennete_logement,
                                                                 value=Unit() if infos_specifiques.ameliore_par_occupant is None else infos_specifiques.ameliore_par_occupant),
                                pret=Pret(
                                    type_pret=TypePret(
                                        code=infos_specifiques.type_pret, value=Unit()),
                                    date_signature=date_of_datetime(
                                        infos_specifiques.date_signature_pret),
                                    titulaire_pret=TitulairePret(
                                        code=infos_specifiques.titulaire_pret, value=Unit())
                                )
                            ) if isinstance(infos_specifiques, InfosAccessionPropriete)
                            else None  # type: ignore
                        )))
                ),
                proprietaire=ParentOuAutre(
                    code=ParentOuAutre_Code.Autre if parts_logement_propriete_famille is None else ParentOuAutre_Code.DemandeurOuConjointOuParentOuViaPartsSocietes,
                    value=Unit() if parts_logement_propriete_famille is None else parts_logement_propriete_famille),
                usufruit=ParentOuAutre(
                    code=ParentOuAutre_Code.Autre if parts_logement_usufruits_famille is None else ParentOuAutre_Code.DemandeurOuConjointOuParentOuViaPartsSocietes,
                    value=Unit() if parts_logement_usufruits_famille is None else parts_logement_usufruits_famille),
                loue_ou_sous_loue_a_des_tiers=LoueOuSousLoueADesTiers(
                    code=LoueOuSousLoueADesTiers_Code.Non if date_naissance_et_conformite_sous_locataire_tiers is None else LoueOuSousLoueADesTiers_Code.Oui,
                    value=Unit() if date_naissance_et_conformite_sous_locataire_tiers is None else PersonneSousLocation(
                        date_naissance_personne_sous_location=date_of_datetime(date_naissance_et_conformite_sous_locataire_tiers[
                            0]),
                        conforme_article_l442_1=date_naissance_et_conformite_sous_locataire_tiers[
                            1]
                    )
                ),
                logement_decent_l89_462=logement_est_decent,
                surface_m_carres=integer_of_int(surface_logement_m_carres),
                zone=ZoneDHabitation(code=zone, value=Unit())
            ),
            personnes_a_charge=[
                (PersonneACharge(code=PersonneACharge_Code.EnfantACharge,
                                 value=EnfantACharge(
                                     identifiant=integer_of_int(
                                         personne_a_charge.identifiant),
                                     beneficie_titre_personnel_aide_personnelle_logement=personne_a_charge.beneficie_titre_personnel_aide_personnelle_logement,
                                     a_deja_ouvert_droit_aux_allocations_familiales=personne_a_charge.a_deja_ouvert_droit_aux_allocations_familiales,
                                     date_de_naissance=date_of_datetime(
                                         personne_a_charge.date_de_naissance[0]),
                                     remuneration_mensuelle=money_of_decimal(
                                         decimal_of_float(personne_a_charge.remuneration_mensuelle)),
                                     obligation_scolaire=SituationObligationScolaire(
                                         code=personne_a_charge.obligation_scolaire, value=Unit()),
                                     situation_garde_alternee=SituationGardeAlternee(code=personne_a_charge.situation_garde_alternee,
                                                                                     value=Unit() if personne_a_charge.coefficient_garde_alternee is None else personne_a_charge.coefficient_garde_alternee)
                                 ))
                 if isinstance(personne_a_charge, EnfantAPL)
                 else (PersonneACharge(
                     code=PersonneACharge_Code.AutrePersonneACharge,
                     value=AutrePersonneACharge(
                         date_naissance=date_of_datetime(
                             personne_a_charge.date_naissance),
                         ressources=money_of_decimal(
                             decimal_of_float(personne_a_charge.ressources)),
                         ascendant_descendant_collateral_deuxieme_troisieme_degre=personne_a_charge.ascendant_descendant_collateral_deuxieme_troisieme_degre,
                         incapacite_80_pourcent_ou_restriction_emploi=personne_a_charge.incapacite_80_pourcent_ou_restriction_emploi,
                         beneficiaire_l161_19_l351_8_l643_3_secu=personne_a_charge.beneficiaire_l161_19_l351_8_l643_3_secu,
                         titulaire_allocation_personne_agee=personne_a_charge.titulaire_allocation_personne_agee,
                         parente=Parente(
                             code=personne_a_charge.parente, value=Unit())
                     )) if isinstance(personne_a_charge, ParentAPL)
                     else None  # type: ignore
                )) for personne_a_charge in personnes_a_charge],
            nombre_autres_occupants_logement=integer_of_int(
                nombre_autres_occupants_logement_hors_menage),
            situation_familiale=SituationFamiliale(
                code=situation_familiale,
                value=Unit() if date_mariage is None else date_of_datetime(date_mariage)
            ),
            condition_rattache_foyer_fiscal_parent_ifi=personne_rattache_foyer_fiscal_parent_ifi,
            enfant_a_naitre_apres_quatrieme_mois_grossesse=enfant_a_naitre_apres_quatrieme_mois_grossesse
        ),
        demandeur_in=Demandeur(
            date_naissance=date_of_datetime(date_naissance_demandeur),
            nationalite=Nationalite(code=nationalite_demandeur, value=Unit()),
            patrimoine=Patrimoine(
                produisant_revenu_periode_r822_3_3_r822_4=money_of_decimal(decimal_of_float(
                    patrimoine_produisant_revenu)),
                ne_produisant_pas_revenu_periode_r822_3_3_r822_4=money_of_decimal(decimal_of_float(
                    patrimoine_ne_produisant_pas_revenu))
            ),
            personne_hebergee_centre_soin_l_L162_22_3_securite_sociale=personne_hebergee_centre_soins,
        ),
        date_courante_in=date_of_datetime(date_courante),
        ressources_menage_prises_en_compte_in=money_of_decimal(
            decimal_of_float(ressources_menage_prises_en_compte)),
    ))
