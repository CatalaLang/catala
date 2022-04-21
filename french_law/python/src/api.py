from catala.runtime import *
from .allocations_familiales import Collectivite, Collectivite_Code, InterfaceAllocationsFamilialesIn, PriseEnCharge, interface_allocations_familiales, PriseEnCharge_Code, EnfantEntree, InterfaceAllocationsFamilialesIn


class Enfant:
    def __init__(
        self,
        id: int,
        remuneration_mensuelle: int,
        date_de_naissance: datetime.date,
        prise_en_charge: PriseEnCharge_Code,
        a_deja_ouvert_droit_aux_allocations_familiales: bool
    ) -> None:
        self.id = id
        self.remuneration_mensuelle = remuneration_mensuelle
        self.date_de_naissance = date_de_naissance
        self.prise_en_charge = prise_en_charge
        self.a_deja_ouvert_droit_aux_allocations_familiales = a_deja_ouvert_droit_aux_allocations_familiales

    def to_allocations_familiales(self) -> EnfantEntree:
        return EnfantEntree(
            d_identifiant=integer_of_int(self.id),
            d_remuneration_mensuelle=money_of_units_int(
                self.remuneration_mensuelle),
            d_a_deja_ouvert_droit_aux_allocations_familiales=self.a_deja_ouvert_droit_aux_allocations_familiales,
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
