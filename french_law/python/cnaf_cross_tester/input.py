from abc import ABC, abstractmethod
from enum import Enum
from typing import Any, List, Optional

class Logement_Code(Enum):
    CodeAppartementOuMaison = 0
    CodeLogementCrous = 1
    CodeLogementFoyer = 2
    CodeLogementResidenceSocialeFJT = 3
    CodeLogementMaisonRetraite = 4
    CodeLogementChambre = 5

class Logement(ABC):
    @abstractmethod
    def residence(self) -> str:
        pass

    @abstractmethod
    def code(self) -> Logement_Code:
        pass

    @abstractmethod
    def typ(self) -> Optional[str]:
        pass

    @abstractmethod
    def meublee(self) -> Optional[bool]:
        pass

    @abstractmethod
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
        self.code_v = Logement_Code.CodeAppartementOuMaison

    def residence(self) -> str:
        return self.residence_v

    def code(self) -> Logement_Code:
        return self.code_v

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
        self.code_v = Logement_Code.CodeLogementCrous

    def residence(self) -> str:
        return self.residence_v

    def code(self) -> Logement_Code:
        return self.code_v

    def typ(self) -> Optional[str]:
        return self.typ_v.value

    def meublee(self) -> Optional[bool]:
        return None

    def __str__(self) -> str:
        return "LogementCrous(typ={})".format(self.typ_v.name)


class LogementFoyer(Logement):
    def __init__(self):
        self.residence_v = 'FOYER'
        self.code_v = Logement_Code.CodeLogementFoyer

    def residence(self) -> str:
        return self.residence_v

    def code(self) -> Logement_Code:
        return self.code_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return None

    def __str__(self) -> str:
        return "LogementFoyer"


class LogementResidenceSocialeFJT(Logement):
    def __init__(self):
        self.residence_v = 'RESIDENCE_SOCIALE_FJT'
        self.code_v = Logement_Code.CodeLogementResidenceSocialeFJT

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def code(self) -> Logement_Code:
        return self.code_v

    def meublee(self) -> Optional[bool]:
        return None

    def __str__(self) -> str:
        return "LogementResidenceSocialeFJT"


class LogementMaisonRetraite(Logement):
    def __init__(self, conventionne : bool):
        self.residence_v = 'MAISON_RETRAITE_EHPAD'
        self.code_v = Logement_Code.CodeLogementMaisonRetraite
        self.conventionne = conventionne

    def residence(self) -> str:
        return self.residence_v

    def code(self) -> Logement_Code:
        return self.code_v

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
        self.code_v = Logement_Code.CodeLogementChambre

    def residence(self) -> str:
        return self.residence_v

    def code(self) -> Logement_Code:
        return self.code_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return self.meuble_v

    def __str__(self) -> str:
        return "LogementChambre(meuble={})".format(self.meuble_v)


class SeulOuCouple(Enum):
    Seul = 'CEL'
    EnCouple = 'VIM'


class Enfant():
    def __init__(self,
                 age: int):
        self.age = age

    def __str__(self) -> str:
        return "Enfant(age={})".format(self.age)


# Le simulateur de la CNAF prend les codes INSEE des communes
class Zone(Enum):
    Zone1 = "75101"
    Zone2 = "69381"
    Zone3 = "46201"


class CnafSimulatorInput():
    def __init__(self,
                 zone: Zone,
                 logement: Logement,
                 loyer: int,
                 seul_ou_couple: SeulOuCouple,
                 enfants: List[Enfant],
                 revenu_pris_en_compte: int):
        self.zone = zone
        self.logement = logement
        self.loyer = loyer
        self.seul_ou_couple = seul_ou_couple
        self.enfants = enfants
        self.revenu_pris_en_compte = revenu_pris_en_compte

    def __str__(self):
        return "-> Code postal : {}\n-> Logement : {}\n-> Loyer : {} €\n-> Seul of couple : {}\n-> Enfants :\n{}\n-> Revenus pris en compte : {} €".format(
            self.zone.name,
            self.logement,
            self.loyer,
            self.seul_ou_couple.name,
            "\n".join(["{}".format(enfant) for enfant in self.enfants]),
            self.revenu_pris_en_compte
        )
