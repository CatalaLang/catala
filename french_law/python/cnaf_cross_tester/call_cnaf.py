# This code is ported from https://github.com/jboillot/apl-fetcher

from .input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, LogementCrousType, LogementCrous, SeulOuCouple
import json
import requests  # type: ignore


def get_bearer():
    url = "https://wwwd.caf.fr/wps/s/GenerateTokenJwtPublic/"

    payload = {}
    headers = {
        'Accept': 'application/json, text/plain, */*',
        'Connection': 'keep-alive',
        'Referer': 'https://wwwd.caf.fr/wps/portal/caffr/aidesetdemarches/mesdemarches/faireunesimulation/lelogement',
        'Sec-Fetch-Dest': 'empty',
        'Sec-Fetch-Mode': 'cors',
        'Sec-Fetch-Site': 'same-origin',
        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36',
        'Cookie': 'DYN_PERSYS=438624522.14119.0000; PERSYS=2801671596.47138.0000; TS015047ff=015e43680bb2f0a3e2996d95996eaa92a52fbad1221c1d64a9f10e25a1c12f8c7ec932a9aff3ba4fd7bdd4441e48f788df0b32610f'
    }

    response = requests.request("GET", url, headers=headers, data=payload)
    data = json.loads(response.text)
    return data['cnafTokenJwt']


def get_simulation(bearer, payload_json):
    import requests

    url = "https://wwwd.caf.fr/api/prestationalmiddle/v1/simuler"

    payload = json.dumps(payload_json)
    headers = {
        'Accept': 'application/json, text/plain, */*',
        'Accept-Language': 'en-GB,en-US;q=0.9,en;q=0.8,fr;q=0.7',
        'Authorization': bearer,
        'Connection': 'keep-alive',
        'Content-Type': 'application/json; charset=UTF-8',
        'Origin': 'https://wwwd.caf.fr',
        'Referer': 'https://wwwd.caf.fr/wps/portal/caffr/aidesetdemarches/mesdemarches/faireunesimulation/lelogement',
        'Sec-Fetch-Dest': 'empty',
        'Sec-Fetch-Mode': 'cors',
        'Sec-Fetch-Site': 'same-origin',
        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36',
        # 'Cookie': 'DYN_PERSYS=438624522.14119.0000; PERSYS=2801671596.47138.0000; TS015047ff=015e43680bb2f0a3e2996d95996eaa92a52fbad1221c1d64a9f10e25a1c12f8c7ec932a9aff3ba4fd7bdd4441e48f788df0b32610f'
    }

    response = requests.request("POST", url, headers=headers, data=payload)
    response_json = response.json()
    # print(response_json)
    if response_json['possedeDesDroits']:
        return int(response_json['montantDroit'])
    else:
        return 0


def format_payload(input: CnafSimulatorInput):
    # C'est un exemple de fichier attendu
    # Les variables parlent d'elles mêmes et ont un type contraint
    flgLogementConventionne = False
    if isinstance(input.logement, LogementCrous):
        if input.logement.typ_v == LogementCrousType.Chambre_rehabilitee:
            flgLogementConventionne = False

    if input.logement.meublee() is None:
        flgMeubleLogement = False
    else:
        flgMeubleLogement = input.logement.meublee()

    logement = {
        "adresse": {
            "codeInsee": str(input.zone.value)
        },
        "loyer": {
            "mtLoyerFoyer": float(input.loyer)
        },
        # parmi ["APPARTEMENT_OU_MAISON", "FOYER", "LOGEMENT_CROUS",
        "cdTypeLogement": str(input.logement.residence_v),
        # "MAISON_RETRAITE_EHPAD", "RESIDENCE_SOCIALE_FJT", "CHAMBRE"]
        "flgLogementConventionne": flgLogementConventionne,
        "flgMeubleLogement": flgMeubleLogement
    }
    personne1 = {
        "personne": {
            "enActivite": False,
            "estAuChomage": False,
            "flgDemandeur": True,  # Est ce la personne demandeuse ?
            "situationProfessionnelleList": [
                    {
                        "situationProfessionnelle": "ACT_INCONNUE"
                        # parmi "ETU" (étudiant non boursier)
                        # "CONGE_MATPAT",
                        # "ST_RSA",
                        # "EBO" (étudiant boursier)
                        # "ACT_INCONNUE" (autre situation)
                        # Et pour les enfants ça peut être
                        # "ETU"
                        # "APPRENTI"
                        # "SALARIE_CP"
                    }
            ],
            "ressourceList": [
                {
                    "groupeOrigine": "salaire",
                    # parmi "salaire", "allocation_chomage",
                    # "retraite_pension_imposable", "pension_alimentaire_recue"
                    # "revenu_travailleur_independant"
                    "mtRessource":  int(float(input.revenu_pris_en_compte) / 0.9)
                },

            ]
        }
    }
    household_members = [personne1]
    if input.seul_ou_couple == SeulOuCouple.Seul:
        personne2 = None
    else:
        personne2 = {
            "personne": {
                "flgDemandeur": False,
                "situationProfessionnelleList": [
                    {
                        "situationProfessionnelle": "ACT_INCONNUE"
                    }
                ],
                "ressourceList": []
            }
        }
        household_members.append(personne2)
    children = [
        {
            "role": "ENF",
            "personne": {
                "role": "ENF",
                "enfantACharge": enfant.age <= 21 and
                    enfant.remuneration_derniere_annee <= 4500,
                "flgDemandeur": False,
                "situationProfessionnelleList": [
                    {
                        "situationProfessionnelle": "ETU"
                    }
                ],
                "ressourceList": []
            }
        }
        for enfant in input.enfants
    ]
    household_members.extend(children)

    flgColocation = False
    if isinstance(input.logement, AppartementOuMaison):
        if input.logement.typ_v == AppartementOuMaisonType.Colocation:
            flgColocation = True
    result = {
        "rattachementLogementList": [
            {
                "flgColocation": flgColocation,
                "logement": logement
            }
        ],
        "compoFoyerList": household_members,
        "situationFamilialeList": [
            {
                "estEnCouple": True if input.seul_ou_couple == SeulOuCouple.EnCouple else False,
                "situationFamille": str(input.seul_ou_couple.value)
                # Parmi "CEL" (célibataire)
                # "VIM" (vie maritale = couple)
            }
        ]}
    return result


def run_simulator(input: CnafSimulatorInput) -> int:
    payload = format_payload(input)
    # print(payload)
    bearer = get_bearer()
    return (get_simulation(bearer, payload))
