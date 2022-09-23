from abc import ABC
from enum import Enum
from typing import Any, List, Optional


class Logement(ABC):
    def residence(self) -> str:
        pass

    def typ(self) -> Optional[str]:
        pass

    def meublee(self) -> Optional[bool]:
        pass

    def __str__(self) -> str:
        pass


class AppartementOuMaisonType(Enum):
    Location = "LOCATION"
    Colocation = "COLOCATION"


class AppartementOuMaison(Logement):
    def __init__(self, typ: AppartementOuMaisonType, meuble: bool):
        self.residence_v = "APPARTEMENT_OU_MAISON"
        self.typ_v = typ
        self.meuble_v = meuble

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return self.typ_v.value

    def meublee(self) -> Optional[bool]:
        return self.meuble_v

    def __str__(self) -> str:
        return "AppartementOuMaison(typ={},meuble={})".format(self.typ_v.name, self.meuble_v)


class LogementCrousType(Enum):
    Chambre = 'CHAMBRE'
    Chambre_rehabilitee = 'CHAMBRE_REHABILITEE'
    Studio = 'STUDIO'


class LogementCrous(Logement):
    def __init__(self, typ: LogementCrousType):
        self.residence_v = 'LOGEMENT_CROUS'
        self.typ_v = typ

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return self.typ_v.value

    def meublee(self) -> Optional[bool]:
        return None

    def __str__(self) -> str:
        return "LogementCrous(typ={})".format(self.typ_v.name)


class LogementFoyer(Logement):
    def __init__(self):
        self.residence_v = 'FOYER'

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return None

    def __str__(self) -> str:
        return "LogementFoyer"


class LogementResidenceSocialeFJT(Logement):
    def __init__(self):
        self.residence_v = 'RESIDENCE_SOCIALE_FJT'

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return None

    def __str__(self) -> str:
        return "LogementResidenceSocialeFJT"


class LogementMaisonRetraite(Logement):
    def __init__(self):
        self.residence_v = 'MAISON_RETRAITE_EHPAD'

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return None

    def __str__(self) -> str:
        return "LogementMaisonRetraite"


class LogementChambre(Logement):
    def __init__(self, meuble: bool):
        self.residence_v = 'CHAMBRE'
        self.meuble_v = meuble

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return self.meuble_v

    def __str__(self) -> str:
        return "LogementChambre(meuble={})".format(self.meuble_v)


class SeulOuCouple(Enum):
    Seul = 'SEUL'
    EnCouple = 'COUPLE'


class Enfant():
    def __init__(self,
                 age: int,
                 remuneration_derniere_annee: int):
        self.age = age
        self.remuneration_derniere_annee = remuneration_derniere_annee

    def __str__(self) -> str:
        return "Enfant(age={},remuneration_derniere_annee={})".format(self.age, self.remuneration_derniere_annee)


class CnafSimulatorInput():
    def __init__(self,
                 code_postal: str,
                 logement: Logement,
                 loyer: int,
                 seul_ou_couple: SeulOuCouple,
                 enfants: List[Enfant],
                 revenu_pris_en_compte: int):
        self.code_postal = code_postal
        self.logement = logement
        self.loyer = loyer
        self.seul_ou_couple = seul_ou_couple
        self.enfants = enfants
        self.revenu_pris_en_compte = revenu_pris_en_compte

    def __str__(self):
        return "-> Code postal : {}\n-> Logement : {}\n-> Loyer : {} €\n-> Seul of couple : {}\n-> Enfants:\n{}\n-> Revenus pris en compte : {} €".format(
            self.code_postal,
            self.logement,
            self.loyer,
            self.seul_ou_couple.name,
            "\n".join(["{}".format(enfant) for enfant in self.enfants]),
            self.revenu_pris_en_compte
        )
