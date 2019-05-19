import unittest
from Complex import Complex


class TestComplex(unittest.TestCase):

    def setUp(self):
        self.a = Complex(10, 1)
        self.b = Complex(0, -5)
        self.c = Complex(1, 2)
        self.d = Complex(4, 4)
        self.e = Complex(-6, -1)

    def tearDown(self):
        pass

    def test_addition(self):
        self.assertEqual(self.a + self.b, Complex(10, -4))
        self.assertEqual(self.b + self.a, Complex(10, -4))
        self.assertEqual(self.a + self.c, Complex(11, 3))
        self.assertEqual(self.c + self.a, Complex(11, 3))
        self.assertEqual(self.c + self.d, Complex(5, 6))
        self.assertEqual(self.d + self.e, Complex(-2, 3))

    def test_subtraction(self):
        self.assertEqual(self.a - self.b, Complex(10, 6))
        self.assertEqual(self.b - self.a, Complex(-10, -6))
        self.assertEqual(self.c - self.b, Complex(1, 7))
        self.assertEqual(self.d - self.c, Complex(3, 2))
        self.assertEqual(self.e - self.d, Complex(-10, -5))
    
    def test_scalar_multiplication(self):
        pass
        
    def test_complex_multiplication(self):
        self.assertEqual(self.a @ self.b, Complex(5, -50))
        self.assertEqual(self.b @ self.c, Complex(10, -5))
        self.assertEqual(self.c @ self.d, Complex(-4, 12))
        self.assertEqual(self.d @ self.e, Complex(-20, -28))
        self.assertEqual(self.e @ self.a, Complex(-59, -16))
        
    def test_equality(self):
        self.assertEqual(self.a, Complex(10, 1))
        self.assertEqual(self.b, Complex(0, -5))
        self.assertEqual(self.c, Complex(1, 2))
        self.assertEqual(self.d, Complex(4, 4))
        self.assertEqual(self.e, Complex(-6, -1))
        
        self.assertEqual(self.a, self.a)
        self.assertEqual(self.b, self.b)
        self.assertEqual(self.c, self.c)
        self.assertEqual(self.d, self.d)
        self.assertEqual(self.e, self.e)
        
        self.assertNotEqual(self.a, self.b)
        self.assertNotEqual(self.b, self.c)
        self.assertNotEqual(self.c, self.d)
        self.assertNotEqual(self.d, self.e)
        self.assertNotEqual(self.e, self.a)
