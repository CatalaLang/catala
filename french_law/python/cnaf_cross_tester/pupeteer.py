# This code is ported from https://github.com/jboillot/apl-fetcher

from playwright.sync_api import sync_playwright
from input import AppartementOuMaison, AppartementOuMaisonType, CnafSimulatorInput

HOME_PAGE = 'https://wwwd.caf.fr/wps/portal/caffr/aidesetservices/lesservicesenligne/estimervosdroits/lelogement'


def run_simulator(input: CnafSimulatorInput):
    with sync_playwright() as p:
        browser = p.firefox.launch(headless=False, slow_mo=10)
        page = browser.new_page()

        # Go to the CNAF simulator
        page.goto(HOME_PAGE)

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

        page.wait_for_timeout(1000)

        # Select residence type
        type_residence_button = page.wait_for_selector(
            "label[for=\"nature_{}\"]".format(input.logement.residence()))
        if type_residence_button is None:
            raise RuntimeError
        type_residence_button.click()

        page.wait_for_timeout(1000)

        browser.close()


run_simulator(
    CnafSimulatorInput(
        code_postal="69001",
        logement=AppartementOuMaison(AppartementOuMaisonType.Location)
    ))
