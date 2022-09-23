# This code is ported from https://github.com/jboillot/apl-fetcher

from pdb import runeval
from playwright.sync_api import sync_playwright
from input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, Enfant, LogementCrous, SeulOuCouple
import re

HOME_PAGE = 'https://wwwd.caf.fr/wps/portal/caffr/aidesetservices/lesservicesenligne/estimervosdroits/lelogement'


def run_simulator(input: CnafSimulatorInput) -> int:
    with sync_playwright() as p:
        browser = p.firefox.launch()  # headless=False, slow_mo=1500)
        page = browser.new_page()

        # Go to the CNAF simulator
        page.goto(HOME_PAGE)

        # Click on cookie banner
        cookie_button = page.wait_for_selector(
            "div[id=\"popup-accept-cookies\"] >> button")
        if cookie_button is None:
            raise RuntimeError
        cookie_button.click()

        # Click on 'Commencer'
        commencer_button = page.wait_for_selector('button[id="btn-commencer"]')
        if commencer_button is None:
            raise RuntimeError
        commencer_button.click()

        # Code Postal
        code_postal_input = page.wait_for_selector('input[id="codePostal"]')
        if code_postal_input is None:
            raise RuntimeError
        code_postal_input.fill(input.code_postal)

        # Select first match
        page.wait_for_selector('li[class="uib-typeahead-match"]')
        page.keyboard.press('Enter')

        # Select residence type
        type_residence_button = page.wait_for_selector(
            "label[for=\"nature_{}\"]".format(input.logement.residence()))
        if type_residence_button is None:
            raise RuntimeError
        type_residence_button.click()

        # Select housing type for some things
        if isinstance(input.logement, AppartementOuMaison):
            type_appartement_ou_maison_button = page.wait_for_selector(
                "label[for=\"statut_{}\"]".format(input.logement.typ()))
            if type_appartement_ou_maison_button is None:
                raise RuntimeError
            type_appartement_ou_maison_button.click()
        elif isinstance(input.logement, LogementCrous):
            type_crous_button = page.wait_for_selector(
                "label[for=\"typeLocal_{}\"]".format(input.logement.typ()))
            if type_crous_button is None:
                raise RuntimeError
            type_crous_button.click()

        # Is the location meublee

        if not (input.logement.meublee() is None):
            meuble_button = page.wait_for_selector(
                "label[for=\"estMeuble_{}\"]".format("true" if input.logement.meublee() else "false"))
            if meuble_button is None:
                raise RuntimeError
            meuble_button.click()

        # Monthly rent
        loyer_mensuel_input = page.wait_for_selector(
            'input[name="montantLoyer"]')
        if loyer_mensuel_input is None:
            raise RuntimeError
        loyer_mensuel_input.fill("{}".format(input.loyer))

        # Couple or not
        seul_button = page.wait_for_selector(
            "label[for=\"situationFamiliale_{}\"]".format(input.seul_ou_couple.value))
        if seul_button is None:
            raise RuntimeError
        seul_button.click()

        # Number of children
        if len(input.enfants) < 0 or len(input.enfants) > 20:
            raise RuntimeError
        plus_button = page.wait_for_selector('button:has-text("+")')
        if plus_button is None:
            raise RuntimeError
        for i in range(len(input.enfants)):
            plus_button.click()

        # Main applicant salary
        salaires_button = page.wait_for_selector(
            "button[id=\"allocataire_siSalaire\"]")
        if salaires_button is None:
            raise RuntimeError
        salaires_button.click()
        salaires_input = page.wait_for_selector(
            "input[id=\"ressourceallocataire_SALAIRE\"]")
        if salaires_input is None:
            raise RuntimeError
        salaires_input.fill("{}".format(
            int(float(input.revenu_pris_en_compte) / 0.9)))
        # We divide by 0.9 because salaries have a 10% franchise
        # Now if the salary is too low you have to click another button
        status_selector = "cnaf-personne[profil=\"'ALLOCATAIRE'\"] >> label[for=\"aucunRevenuAUTRE_SITUATION\"]"
        if not (page.query_selector is None):
            status_button = page.wait_for_selector(status_selector)
            if status_button is None:
                raise RuntimeError
            status_button.click()

        # If there is a partner we assume no income for simplicity
        if input.seul_ou_couple == SeulOuCouple.EnCouple:
            sans_button = page.wait_for_selector(
                "button[id=\"conjoint_siAucunRevenu\"]")
            if sans_button is None:
                raise RuntimeError
            sans_button.click()
            status_button = page.wait_for_selector(
                "cnaf-personne[profil=\"'CONJOINT'\"] >> label[for=\"aucunRevenuAUTRE_SITUATION\"]")
            if status_button is None:
                raise RuntimeError
            status_button.click()

        # Continue
        continuer_button = page.wait_for_selector('button[id="btn-suivant"]')
        if continuer_button is None:
            raise RuntimeError
        continuer_button.click()

        # Extra questions about children
        if len(input.enfants) > 0:
            page.wait_for_selector(
                selector="div[ng-form=\"vm.questionsEnfantsForm\"]", state='attached')
            i = 0
            for element in page.query_selector_all("cnaf-personne[profil=\"'ENFANT'\"]"):
                if input.enfants[i].age < 21:
                    yes_button = element.wait_for_selector(
                        "label[for=\"personneAGE_MINIMUM\"][uib-btn-radio=\"true\"]")
                    if yes_button is None:
                        raise RuntimeError
                    yes_button.click()
                else:
                    no_button = element.wait_for_selector(
                        "label[for=\"personneAGE_MINIMUM\"][uib-btn-radio=\"false\"]")
                    if no_button is None:
                        raise RuntimeError
                    no_button.click()
                if int(float(input.enfants[i].remuneration_derniere_annee) / 0.9) > 4500:
                    yes_button = element.wait_for_selector(
                        "label[for=\"ENFANT_MONTANT_MAX\"][uib-btn-radio=\"true\"]")
                    if yes_button is None:
                        raise RuntimeError
                    yes_button.click()
                    # We have to provide the exact remuneration
                    salaires_button = element.wait_for_selector(
                        "label[for=\"enfant_{}_siSalaire\"]".format(i))
                    if salaires_button is None:
                        raise RuntimeError
                    salaires_button.click()
                    salaires_input = element.wait_for_selector(
                        "input[id=\"ressourceenfant_{}_SALAIRE\"]".format(i))
                    if salaires_input is None:
                        raise RuntimeError
                    salaires_input.fill("{}".format(
                        int(float(input.enfants[i].remuneration_derniere_annee) / 0.9)))
                    salaires_status = element.wait_for_selector(
                        "label[for=\"enfantSalarieSALARIE\"]")
                    if salaires_status is None:
                        raise RuntimeError
                    salaires_status.click()
                else:
                    no_button = element.wait_for_selector(
                        "label[for=\"ENFANT_MONTANT_MAX\"][uib-btn-radio=\"false\"]")
                    if no_button is None:
                        raise RuntimeError
                    no_button.click()
                i += 1

            # Continue
            continuer_button = page.wait_for_selector(
                'button[id="btn-suivant"]')
            if continuer_button is None:
                raise RuntimeError
            continuer_button.click()

        # Retrieve the amount
        page.wait_for_selector('section[id="resultat"]')
        result = page.locator('text=/\\d+€ par mois/').text_content()
        if result is None:
            raise RuntimeError

        match = re.search("(\\d+)€ par mois", result)
        if match is None:
            raise RuntimeError
        housing_benefits = match.group(1)
        if housing_benefits is None:
            raise RuntimeError

        browser.close()
        return int(housing_benefits)


input = CnafSimulatorInput(
    code_postal="69001",
    logement=AppartementOuMaison(
        AppartementOuMaisonType.Location, meuble=False),
    loyer=450,
    seul_ou_couple=SeulOuCouple.EnCouple,
    enfants=[Enfant(age=7, remuneration_derniere_annee=0),
             Enfant(age=14, remuneration_derniere_annee=5000)],
    revenu_pris_en_compte=11_500
)
print(input)
housing_benefits = run_simulator(input)
print("Aides au logement : {} €".format(housing_benefits))
