# This code is ported from https://github.com/jboillot/apl-fetcher

from pdb import runeval
from playwright.sync_api import sync_playwright
from input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput, Enfant, LogementCrous, SeulOuCouple
import re

HOME_PAGE = 'https://wwwd.caf.fr/wps/portal/caffr/aidesetservices/lesservicesenligne/estimervosdroits/lelogement'


def run_simulator(input: CnafSimulatorInput) -> int:
    with sync_playwright() as p:
        browser = p.firefox.launch(headless=False, slow_mo=100)
        page = browser.new_page()

        # Go to the CNAF simulator
        page.goto(HOME_PAGE)

        # Click on cookie banner
        cookie_button = page.wait_for_selector(
            "div[id=\"popup-accept-cookies\"] >> button")
        if cookie_button is None:
            raise RuntimeError
        cookie_button.click(force=True)

        # Click on 'Commencer'
        commencer_button = page.wait_for_selector('button[id="btn-suivant"]')
        if commencer_button is None:
            raise RuntimeError
        commencer_button.click(force=True)

        # Code Postal
        code_postal_input = page.wait_for_selector('input[id="cpCommune"]')
        if code_postal_input is None:
            raise RuntimeError
        code_postal_input.fill(input.zone.value)

        # Select first match
        page.wait_for_selector('a[class="ng-tns-c31-0"]')
        page.keyboard.press('Enter')

        # Select residence type
        type_residence_button = page.wait_for_selector(
            "input[id=\"lieuResidence_{}\"]".format(input.logement.residence()))
        if type_residence_button is None:
            raise RuntimeError
        type_residence_button.click(force=True)

        # Select housing type for some things
        if isinstance(input.logement, AppartementOuMaison):
            type_appartement_ou_maison_button = page.wait_for_selector(
                "input[id=\"typeOccupation_{}\"]".format(input.logement.typ()))
            if type_appartement_ou_maison_button is None:
                raise RuntimeError
            type_appartement_ou_maison_button.click(force=True)
        elif isinstance(input.logement, LogementCrous):
            type_crous_button = page.wait_for_selector(
                "input[id=\"typeCrous_{}\"]".format(input.logement.typ()))
            if type_crous_button is None:
                raise RuntimeError
            type_crous_button.click(force=True)

        # Is the location meublee

        if not (input.logement.meublee() is None):
            meuble_button = page.wait_for_selector(
                "input[id=\"estMeuble_{}\"]".format("true" if input.logement.meublee() else "false"))
            if meuble_button is None:
                raise RuntimeError
            meuble_button.click(force=True)

        # Monthly rent
        loyer_mensuel_input = page.wait_for_selector(
            'input[id="mttDeclare"]')
        if loyer_mensuel_input is None:
            raise RuntimeError
        loyer_mensuel_input.fill("{}".format(input.loyer))

        # Couple or not
        seul_button = page.wait_for_selector(
            "input[id=\"situationFamiliale_{}\"]".format(input.seul_ou_couple.value))
        if seul_button is None:
            raise RuntimeError
        seul_button.click(force=True)

        # Number of children
        if len(input.enfants) < 0 or len(input.enfants) > 20:
            raise RuntimeError
        plus_button = page.wait_for_selector('button:has-text("+")')
        if plus_button is None:
            raise RuntimeError
        for i in range(len(input.enfants)):
            plus_button.click(force=True)

        # Main applicant salary
        salaires_button = page.wait_for_selector(
            "button[id=\"SALAIRE_0_checkbox\"]")
        if salaires_button is None:
            raise RuntimeError
        salaires_button.click(force=True)
        salaires_input = page.wait_for_selector(
            "input[id=\"SALAIRE_0_montant\"]")
        if salaires_input is None:
            raise RuntimeError
        salaires_input.fill("{}".format(
            int(float(input.revenu_pris_en_compte) / 0.9)))
        salaires_input.wait_for_element_state(state="stable")
        # We divide by 0.9 because salaries have a 10% franchise
        # Now if the salary is too low you have to click another button
        status_selector = "input[id=\"sitro_0_SITPRO_ACT_INCONNUE\"]"
        if not (page.query_selector is None):
            status_button = page.wait_for_selector(status_selector)
            if status_button is None:
                raise RuntimeError
            status_button.click(force=True)

        # If there is a partner we assume no income for simplicity
        if input.seul_ou_couple == SeulOuCouple.EnCouple:
            sans_button = page.wait_for_selector(
                "button[id=\"aucunRevenu_1\"]")
            if sans_button is None:
                raise RuntimeError
            sans_button.click(force=True)
            status_button = page.wait_for_selector(
                "input[id=\"sitro_1_SITPRO_ACT_INCONNUE\"]")
            if status_button is None:
                raise RuntimeError
            status_button.click(force=True)

        # Continue
        continuer_button = page.wait_for_selector('button[id="btn-suivant"]')
        if continuer_button is None:
            raise RuntimeError
        continuer_button.click(force=True)

        # Extra questions about children
        if len(input.enfants) > 0:
            for i in range(len(input.enfants)):
                if input.enfants[i].age < 21:
                    yes_button = page.wait_for_selector(
                        "input[id=\"QUESTION_AGE_MAX_{}_true\"]".format(i))
                    if yes_button is None:
                        raise RuntimeError
                    yes_button.click(force=True)
                else:
                    no_button = page.wait_for_selector(
                        "input[id=\"QUESTION_AGE_MAX_{}_false\"]".format(i))
                    if no_button is None:
                        raise RuntimeError
                    no_button.click(force=True)
                if int(float(input.enfants[i].remuneration_derniere_annee) / 0.9) > 4500:
                    yes_button = page.wait_for_selector(
                        "input[id=\"QUESTION_REVENUS_DERNIERSMOIS_{}_true\"]".format(i))
                    if yes_button is None:
                        raise RuntimeError
                    yes_button.click(force=True)
                    # We have to provide the exact remuneration
                    salaires_button = page.wait_for_selector(
                        "button[id=\"salaire{}_checkbox\"]".format(i))
                    if salaires_button is None:
                        raise RuntimeError
                    salaires_button.click(force=True)
                    salaires_input = page.wait_for_selector(
                        "input[id=\"MONTANT_salaire{}\"]".format(i))
                    if salaires_input is None:
                        raise RuntimeError
                    salaires_input.fill("{}".format(
                        int(float(input.enfants[i].remuneration_derniere_annee) / 0.9)))
                    salaires_status = page.wait_for_selector(
                        "input[id=\"QUESTION_SITPRO_{}_SALARIE_CP\"]".format(i))
                    if salaires_status is None:
                        raise RuntimeError
                    salaires_status.click(force=True)
                else:
                    no_button = page.wait_for_selector(
                        "input[id=\"QUESTION_REVENUS_DERNIERSMOIS_{}_false\"]".format(i))
                    if no_button is None:
                        raise RuntimeError
                    no_button.click(force=True)

            # Continue
            continuer_button = page.wait_for_selector(
                'button[id="btn-suivant"]')
            if continuer_button is None:
                raise RuntimeError
            continuer_button.click(force=True)

        # Wait for result page
        page.wait_for_selector("section[id=\"resultat\"]")

        # Retrieve the amount
        result = page.query_selector('text=/\\d+ € par mois/').text_content()
        if result is None:
            # Then no benefits!
            housing_benefits = 0
        else:
            match = re.search("(\\d+) € par mois", result)
            if match is None:
                raise RuntimeError
            housing_benefits = match.group(1)
            if housing_benefits is None:
                raise RuntimeError

        browser.close()
        return int(housing_benefits)
