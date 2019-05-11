import unittest
from Matrix import Matrix


class TestMatrix(unittest.TestCase):

    def setUp(self):
        self.a = Matrix([[1]])
        self.b = Matrix([[2]])
        self.c = Matrix([[10]])
        self.d = Matrix([[1, 2], [3, 4]])
        self.e = Matrix([[10, 10], [10, 10]])
        self.f = Matrix([[12, 7, 4], [6, 8, 2]])
        self.g = Matrix([[24, -8, 0], [-1, 2, 2]])
        self.h = Matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
        self.i = Matrix([[5, 3, 8], [6, 1, 1], [9, 5, 7]])

    def tearDown(self):
        pass

    def test_addition(self):
        self.assertEqual(self.a + self.b, Matrix([[3]]))
        self.assertEqual(self.a + self.c, Matrix([[11]]))
        self.assertEqual(self.b + self.c, Matrix([[12]]))

        self.assertEqual(self.d + self.e, Matrix([[11, 12], [13, 14]]))
        self.assertEqual(self.e + self.d, Matrix([[11, 12], [13, 14]]))

        self.assertEqual(self.f + self.g, Matrix([[36, -1, 4], [5, 10, 4]]))

        self.assertEqual(self.h + self.i, Matrix([[6, 5, 11], [10, 6, 7], [16, 13, 16]]))

        with self.assertRaises(ValueError):
            dummy = self.a + self.d
            dummy = self.a + self.e
            dummy = self.h + self.b
            dummy = self.i + self.f

    def test_subtraction(self):
        self.assertEqual(self.a - self.b, Matrix([[-1]]))
        self.assertEqual(self.a - self.c, Matrix([[-9]]))
        self.assertEqual(self.b - self.a, Matrix([[1]]))
        self.assertEqual(self.c - self.b, Matrix([[8]]))
        
        self.assertEqual(self.d - self.e, Matrix([[-9, -8], [-7, -6]]))
        self.assertEqual(self.e - self.d, Matrix([[9, 8], [7, 6]]))
        self.assertEqual(self.g - self.f, Matrix([[12, -15, -4], [-7, -6, 0]]))
        self.assertEqual(self.h - self.i, Matrix([[-4, -1, -5], [-2, 4, 5], [-2, 3, 2]]))
        
        with self.assertRaises(ValueError):
            dummy = self.d - self.a
            dummy = self.g - self.c
            dummy = self.b - self.i
            dummy = self.i - self.f

    def test_scalar_multiplication(self):
        self.assertEqual(Matrix([[5]]), self.a * 5)
        self.assertEqual(Matrix([[20]]), 2 * self.c)
        self.assertEqual(Matrix([[8]]), 4 * self.b)
        self.assertEqual(Matrix([[8]]), self.b * 4)
        
        self.assertEqual(self.d * 10, Matrix([[10, 20], [30, 40]]))
        self.assertEqual(Matrix([[42, 42], [42, 42]]), self.e * 4.2)
        
        self.assertEqual(1.1 * self.f, Matrix([[13.2, 7.7, 4.4], [6.6, 8.8, 2.2]]))
        self.assertEqual(Matrix([[-30, 20, 0], [2.5, -5, -5]]), self.g * (-2.5))
        
        self.assertEqual(self.h * 0.5, Matrix([[0.5, 1, 1.5], [2, 2.5, 3], [3.5, 4, 4.5]]))
        self.assertEqual(self.i * 10, Matrix([[50, 30, 80], [60, 10, 10], [90, 50, 70]]))

    @unittest.SkipTest
    def test_matrix_multiplication(self):
        pass

    @unittest.SkipTest
    def test_equality(self):
        pass

    @unittest.SkipTest
    def test_rows_and_cols(self):
        pass

    @unittest.SkipTest
    def test_inverse(self):
        pass

    @unittest.SkipTest
    def test_transpose(self):
        pass

    @unittest.SkipTest
    def test_trace(self):
        pass

    @unittest.SkipTest
    def test_determinant(self):
        pass

    @unittest.SkipTest
    def test_sub_matrix(self):
        pass

    @unittest.SkipTest
    def test_is_square(self):
        pass

    @unittest.SkipTest
    def test_is_symmetric(self):
        pass
