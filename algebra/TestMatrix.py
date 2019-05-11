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
        pass

    def test_scalar_multiplication(self):
        pass

    def test_matrix_multiplication(self):
        pass

    def test_equality(self):
        pass

    def test_rows_and_cols(self):
        pass

    def test_inverse(self):
        pass

    def test_transpose(self):
        pass

    def test_trace(self):
        pass

    def test_determinant(self):
        pass

    def test_sub_matrix(self):
        pass

    def test_is_square(self):
        pass

    def test_is_symmetric(self):
        pass
