from abc import ABC
from enum import Enum
from typing import Any, Optional


class Logement(ABC):
    def residence(self) -> str:
        pass

    def typ(self) -> Optional[str]:
        pass

    def meublee(self) -> Optional[bool]:
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


class LogementFoyer(Logement):
    def __init__(self):
        self.residence_v = 'FOYER'

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return None


class LogementResidenceSocialeFJT(Logement):
    def __init__(self):
        self.residence_v = 'RESIDENCE_SOCIALE_FJT'

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return None


class LogementMaisonRetraite(Logement):
    def __init__(self):
        self.residence_v = 'MAISON_RETRAITE_EHPAD'

    def residence(self) -> str:
        return self.residence_v

    def typ(self) -> Optional[str]:
        return None

    def meublee(self) -> Optional[bool]:
        return None


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


class CnafSimulatorInput():
    def __init__(self,
                 code_postal: str,
                 logement: Logement):
        self.code_postal = code_postal
        self.logement = logement
