import random
from input import CnafSimulatorInput


def generate_random_input() -> CnafSimulatorInput:
    zone = random.randint(1, 3)
    if zone == 1:
        code_postal = "75000"
    elif zone == 2:
        code_postal = "69000"
    else:  # zone == 3
        code_postal = "46800"
