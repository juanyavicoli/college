import unittest
import math
from Complex import Complex


class TestComplex(unittest.TestCase):

    def setUp(self):
        self.a = Complex(10, 1)
        self.b = Complex(0, -5)
        self.c = Complex(1, 2)
        self.d = Complex(4, 4)
        self.e = Complex(-6, -1)
        self.f = Complex(15, 0)

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
        self.assertEqual(self.a * 1, self.a)
        self.assertEqual(1 * self.a, self.a)
        self.assertEqual(self.b * 2, Complex(0, -10))
        self.assertEqual(2 * self.b, Complex(0, -10))
        self.assertEqual(self.c * 3, Complex(3, 6))
        self.assertEqual(3 * self.c, Complex(3, 6))

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

    def test_real_and_imaginary(self):
        self.assertEqual(self.a.real(), 10)
        self.assertEqual(self.a.imaginary(), 1)
        self.assertEqual(self.b.real(), 0)
        self.assertEqual(self.b.imaginary(), -5)
        self.assertEqual(self.c.real(), 1)
        self.assertEqual(self.c.imaginary(), 2)
        self.assertEqual(self.d.real(), 4)
        self.assertEqual(self.d.imaginary(), 4)
        self.assertEqual(self.e.real(), -6)
        self.assertEqual(self.e.imaginary(), -1)

    def test_modulus(self):
        self.assertEqual(self.a.modulus(), math.sqrt(101))
        self.assertEqual(self.b.modulus(), 5)
        self.assertEqual(self.c.modulus(), math.sqrt(5))
        self.assertEqual(self.d.modulus(), math.sqrt(32))
        self.assertEqual(self.e.modulus(), math.sqrt(37))
        self.assertEqual(self.f.modulus(), 15)

    def test_conjugate(self):
        self.assertEqual(self.a.conjugate(), Complex(10, -1))
        self.assertEqual(self.b.conjugate(), Complex(0, 5))
        self.assertEqual(self.c.conjugate(), Complex(1, -2))
        self.assertEqual(self.d.conjugate(), Complex(4, -4))
        self.assertEqual(self.e.conjugate(), Complex(-6, 1))

    def test_reciprocal(self):
        self.assertEqual(self.a.reciprocal(), Complex(0.099009900990099, -0.0099009900990099))
        self.assertEqual(self.b.reciprocal(), Complex(0, 0.2))
        self.assertEqual(self.c.reciprocal(), Complex(0.2, -0.4))
        self.assertEqual(self.d.reciprocal(), Complex(0.125, -0.125))
        self.assertEqual(self.e.reciprocal(), Complex(-0.16216216216216, 0.027027027027027))
        self.assertEqual(self.f.reciprocal(), Complex(0.066666666666667, 0))

    def test_duplicate(self):
        self.assertEqual(self.a.duplicate(), self.a)
        self.assertEqual(self.b.duplicate(), self.b)
        self.assertEqual(self.c.duplicate(), self.c)
        self.assertEqual(self.d.duplicate(), self.d)
        self.assertEqual(self.e.duplicate(), self.e)
        self.assertEqual(self.f.duplicate(), self.f)

        self.assertEqual(self.a.duplicate(), Complex(10, 1))
        self.assertEqual(self.b.duplicate(), Complex(0, -5))
        self.assertEqual(self.c.duplicate(), Complex(1, 2))
        self.assertEqual(self.d.duplicate(), Complex(4, 4))
        self.assertEqual(self.e.duplicate(), Complex(-6, -1))
        self.assertEqual(self.f.duplicate(), Complex(15, 0))

    def test_is_zero(self):
        self.assertEqual(self.a.is_zero(), False)
        self.assertEqual(self.b.is_zero(), False)
        self.assertEqual(self.c.is_zero(), False)
        self.assertEqual(self.d.is_zero(), False)
        self.assertEqual(self.e.is_zero(), False)
        self.assertEqual(Complex(0, 0).is_zero(), True)

    def test_is_real(self):
        self.assertEqual(self.a.is_real(), False)
        self.assertEqual(self.b.is_real(), False)
        self.assertEqual(self.c.is_real(), False)
        self.assertEqual(self.d.is_real(), False)
        self.assertEqual(self.e.is_real(), False)
        self.assertEqual(self.f.is_real(), True)

    def test_is_pure(self):
        self.assertEqual(self.a.is_pure(), False)
        self.assertEqual(self.b.is_pure(), True)
        self.assertEqual(self.c.is_pure(), False)
        self.assertEqual(self.d.is_pure(), False)
        self.assertEqual(self.e.is_pure(), False)
        self.assertEqual(self.f.is_pure(), False)

