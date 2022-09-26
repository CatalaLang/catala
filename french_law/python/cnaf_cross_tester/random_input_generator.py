import random
from input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, Enfant, Logement, LogementChambre, LogementCrous, LogementCrousType, LogementFoyer, LogementMaisonRetraite, LogementResidenceSocialeFJT, SeulOuCouple, Zone


def generate_random_child() -> Enfant:
    age = random.randint(0, 25)
    remuneration_derniere_annee = random.randint(0, 12 * 1500)
    return Enfant(age, remuneration_derniere_annee)


def generate_random_input() -> CnafSimulatorInput:
    zone_i = random.randint(1, 3)
    if zone_i == 1:
        zone = Zone.Zone1
    elif zone_i == 2:
        zone = Zone.Zone2
    else:  # zone_i == 3
        zone = Zone.Zone3
    loyer = random.randint(300, 1800)
    revenus_pris_en_compte = random.randint(0, 200) * 100
    seul_ou_couple_i = random.randint(1, 2)
    if seul_ou_couple_i == 1:
        seul_ou_couple = SeulOuCouple.Seul
    else:  # seul_ou_couple_i == 2
        seul_ou_couple = SeulOuCouple.EnCouple
    nb_enfants = random.randint(0, 8)
    enfants = [generate_random_child() for i in range(nb_enfants)]
    typ_logement = random.randint(1, 6)
    meuble_i = random.randint(1, 2)
    logement: Logement
    if meuble_i == 1:
        meuble = True
    else:  # meuble_i == 2
        meuble = False
    if typ_logement == 1:
        typ_location_i = random.randint(1, 2)
        if typ_location_i == 1:
            typ_location = AppartementOuMaisonType.Location
        else:  # typ_location_i == 2
            typ_location = AppartementOuMaisonType.Colocation
        logement = AppartementOuMaison(typ_location, meuble)
    elif typ_logement == 2:
        typ_i = random.randint(1, 3)
        if typ_i == 1:
            typ = LogementCrousType.Chambre
        elif typ_i == 2:
            typ = LogementCrousType.Chambre_rehabilitee
        else:  # typ_i == 3
            typ = LogementCrousType.Studio
        logement = LogementCrous(typ)
    elif typ_logement == 3:
        logement = LogementFoyer()
    elif typ_logement == 4:
        logement = LogementResidenceSocialeFJT()
    elif typ_logement == 5:
        logement = LogementMaisonRetraite()
    else:  # typ_logement == 6:
        logement = LogementChambre(meuble)
    return CnafSimulatorInput(
        zone=zone,
        logement=logement,
        loyer=loyer,
        seul_ou_couple=seul_ou_couple,
        enfants=enfants,
        revenu_pris_en_compte=revenus_pris_en_compte
    )
