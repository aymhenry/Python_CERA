# Python Import ==================
import unittest

# User Import ======================
from CoolPrp import *


def setUpModule():
    pass


def tearDownModule():
    pass


class TestName(unittest.TestCase):
    def setUp(self):
        """Method to prepare the test fixture. Run BEFORE the test methods."""

        self.fuild = 'R12'
        self.objCP = CoolPrp()
        self.objCP.setup(self.fuild)  # used Fuild RF12

        self.TK_C = 273.15  # K
        T1 = self.TK_C + 72  # K
        P1 = 200000  # Pa
        # H = 400061.6198132278    # J/kg
        S = 1741.3033288796132  # J/kg/K
        # D = 8.632966162031755  #kg/m3
        V = 0.11583504223589493  # m3/kg

    def tearDown(self):
        """Method to tear down the test fixture. Run AFTER the test methods."""
        pass

    def addCleanup(self, function, *args, **kwargs):
        """Function called AFTER tearDown() to clean resources used on test."""
        pass

    @classmethod
    def setUpClass(cls):
        """Class method called BEFORE tests in an individual class run. """
        pass  # Probably you may not use this one. See setUp().

    @classmethod
    def tearDownClass(cls):
        """Class method called AFTER tests in an individual class run. """
        pass  # Probably you may not use this one. See tearDown().

    def test_CoolPrb(self):
        self.assertEqual(self.objCP.getFuild(), self.fuild)  # a == b

        # Critical properties
        Critical_temperature = 385.12  # K
        Critical_Pressure = 4136100.000000001  # Pa
        Critical_Volume = 565.0000000000001  # kg/m3

        self.assertEqual(self.objCP.getCrtiticalTemp(), Critical_temperature)
        self.assertEqual(self.objCP.getCrtiticalPress(), Critical_Pressure)
        self.assertEqual(self.objCP.getCrtiticalVolume(), Critical_Volume)

        # Saturated condition
        T1 = self.TK_C + 18  # K
        P1 = 535130  # Pa
        Hliq = 217184.70478619912  # the Hliq values

        V_liq = 0.000749  # 0.0007485739495725898 kg/m3
        V_gas = 0.032897  # m3/kg
        D_liq = 1335.87  # kg/m3
        D_gas = 30.398  # m3/kg

        self.assertAlmostEqual(round(self.objCP.Property('V', T=T1, X=0), 6), V_liq)
        self.assertEqual(round(self.objCP.Property('V', T=T1, X=1), 6), V_gas)
        self.assertEqual(round(self.objCP.Property('D', T=T1, X=0), 2), D_liq)
        self.assertEqual(round(self.objCP.Property('D', T=T1, X=1), 3), D_gas)

        self.assertAlmostEqual(round(self.objCP.Property('V', P=P1, X=0), 6), V_liq)
        self.assertEqual(round(self.objCP.Property('V', P=P1, X=1), 6), V_gas)
        self.assertEqual(round(self.objCP.Property('D', P=P1, X=0), 2), D_liq)
        self.assertEqual(round(self.objCP.Property('D', P=P1, X=1), 3), D_gas)

        # Wet arae test
        self.assertEqual(self.objCP.Property('H', T=T1, P=P1), Hliq)  # a == b

        # Phase
        self.assertEqual(self.objCP.phase_byPressTemp(1013250, 273+100),
                         "gas")
        self.assertTrue(
            self.objCP.is_gas_phase(
                self.objCP.phase_byPressTemp(1013250, 273+100)
                                    )
                        )

        self.assertEqual(self.objCP.phase_byPressTemp(1013250, 273+00),
                         "liquid")

        self.assertFalse(
            self.objCP.is_gas_phase(
                    self.objCP.phase_byPressTemp(1013250, 273+00)
                                    )
                        )

        Tsat = 315.149
        Psat = 1006791.617
        self.assertEqual(
                round(self.objCP.Property('T', P=Psat, X=0), 3),
                Tsat
                        )
        
        self.assertEqual(
                round(self.objCP.Property('P', T=Tsat, X=0), 3),
                Psat
                        )

        with self.assertRaises(Exception) as context:
            self.objCP.Property('H', T=Tsat, X=2) # This line  Must raise SomeException   
        
             
        
if __name__.__contains__("__main__"):
    unittest.main()
