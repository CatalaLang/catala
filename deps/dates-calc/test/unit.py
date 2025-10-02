# Run from root directory of dates-calc with `python3 -m unittest test.unit`
import unittest
import csv
from lib_python.src.dates_calc import Date, Period, AbortOnRound, RoundUp, RoundDown, addup, adddown, AmbiguousComputation

class TestDates(unittest.TestCase):
    def read_csv(self, filename, drop_header=True):
        with open(filename) as csvfile:
            csvreader = csv.reader(csvfile, delimiter=';')
            r = []
            if drop_header: next(csvreader)
            for l in csvreader:
                tmp = [c.strip() for c in l]
                r.append(tmp)
            return r

    def test_add_dates_exact(self):
        cases = self.read_csv("test/exact_computations.csv") # unittest are supposed to be run from the root directory
        for c in cases:
            d = Date.from_string(c[0])
            p = Period.from_string(c[1])
            dr = Date.from_string(c[2])

            self.assertEqual(d + p, dr) # AbortOnRound
            self.assertEqual(d |addup| p, dr)
            self.assertEqual(d |adddown| p, dr)

    def test_add_dates_ambiguous(self):
        cases = self.read_csv("test/ambiguous_computations.csv") # unittest are supposed to be run from the root directory
        for c in cases:
            d = Date.from_string(c[0])
            p = Period.from_string(c[1])
            dup = Date.from_string(c[2])
            ddo = Date.from_string(c[3])

            with self.assertRaises(AmbiguousComputation):
                d + p
            self.assertEqual(d |addup| p, dup)
            self.assertEqual(d |adddown| p, ddo)

    def test_first_last_day_of_month(self):
        cases = self.read_csv("test/first_last_day_of_month.csv")
        for c in cases:
            d = Date.from_string(c[0])
            df = Date.from_string(c[1])
            dl = Date.from_string(c[2])

            d_f = d.first_day_of_month()
            d_l = d.last_day_of_month()

            self.assertEqual(df, d_f)
            self.assertEqual(dl, d_l)

if __name__ == "__main__":
    unittest.main(verbosity)
