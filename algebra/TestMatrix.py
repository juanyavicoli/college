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
        self.assertEqual(Matrix([[-60, 20, 0], [2.5, -5, -5]]), self.g * (-2.5))
        
        self.assertEqual(self.h * 0.5, Matrix([[0.5, 1, 1.5], [2, 2.5, 3], [3.5, 4, 4.5]]))
        self.assertEqual(self.i * 10, Matrix([[50, 30, 80], [60, 10, 10], [90, 50, 70]]))

    @unittest.SkipTest
    def test_matrix_multiplication(self):
        pass

    def test_equality(self):
        self.assertEqual(self.a, Matrix([[1]]))
        self.assertEqual(self.b, Matrix([[2]]))
        self.assertEqual(self.c, Matrix([[10]]))
        self.assertEqual(self.d, Matrix([[1, 2], [3, 4]]))
        self.assertEqual(self.e, Matrix([[10, 10], [10, 10]]))
        self.assertEqual(self.f, Matrix([[12, 7, 4], [6, 8, 2]]))
        self.assertEqual(self.g, Matrix([[24, -8, 0], [-1, 2, 2]]))
        self.assertEqual(self.h, Matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]]))
        self.assertEqual(self.i, Matrix([[5, 3, 8], [6, 1, 1], [9, 5, 7]]))

        self.assertEqual(self.a, self.a)
        self.assertEqual(self.b, self.b)
        self.assertEqual(self.c, self.c)
        self.assertEqual(self.d, self.d)
        self.assertEqual(self.e, self.e)
        self.assertEqual(self.f, self.f)
        self.assertEqual(self.g, self.g)
        self.assertEqual(self.h, self.h)
        self.assertEqual(self.i, self.i)

        self.assertNotEqual(self.a, self.b)
        self.assertNotEqual(self.b, self.c)
        self.assertNotEqual(self.c, self.d)
        self.assertNotEqual(self.d, self.e)
        self.assertNotEqual(self.e, self.f)
        self.assertNotEqual(self.f, self.g)
        self.assertNotEqual(self.g, self.h)
        self.assertNotEqual(self.h, self.i)
        self.assertNotEqual(self.i, self.a)

    def test_rows_and_cols(self):
        self.assertEqual(self.a.rows(), 1)
        self.assertEqual(self.b.rows(), 1)
        self.assertEqual(self.c.rows(), 1)
        self.assertEqual(self.d.rows(), 2)
        self.assertEqual(self.e.rows(), 2)
        self.assertEqual(self.f.rows(), 2)
        self.assertEqual(self.g.rows(), 2)
        self.assertEqual(self.h.rows(), 3)
        self.assertEqual(self.i.rows(), 3)

        self.assertEqual(self.a.cols(), 1)
        self.assertEqual(self.b.cols(), 1)
        self.assertEqual(self.c.cols(), 1)
        self.assertEqual(self.d.cols(), 2)
        self.assertEqual(self.e.cols(), 2)
        self.assertEqual(self.f.cols(), 3)
        self.assertEqual(self.g.cols(), 3)
        self.assertEqual(self.h.cols(), 3)
        self.assertEqual(self.i.cols(), 3)

    @unittest.SkipTest
    def test_inverse(self):
        pass

    def test_transpose(self):
        self.assertEqual(self.a.transpose(), Matrix([[1]]))
        self.assertEqual(self.b.transpose(), Matrix([[2]]))
        self.assertEqual(self.c.transpose(), Matrix([[10]]))
        self.assertEqual(self.d.transpose(), Matrix([[1, 3], [2, 4]]))
        self.assertEqual(self.e.transpose(), Matrix([[10, 10], [10, 10]]))
        self.assertEqual(self.f.transpose(), Matrix([[12, 6], [7, 8], [4, 2]]))
        self.assertEqual(self.g.transpose(), Matrix([[24, -1], [-8, 2], [0, 2]]))
        self.assertEqual(self.h.transpose(), Matrix([[1, 4, 7], [2, 5, 8], [3, 6, 9]]))
        self.assertEqual(self.i.transpose(), Matrix([[5, 6, 9], [3, 1, 5], [8, 1, 7]]))

    def test_trace(self):
        self.assertEqual(self.a.trace(), 1)
        self.assertEqual(self.b.trace(), 2)
        self.assertEqual(self.c.trace(), 10)
        self.assertEqual(self.d.trace(), 5)
        self.assertEqual(self.e.trace(), 20)
        self.assertEqual(self.h.trace(), 15)
        self.assertEqual(self.i.trace(), 13)

        with self.assertRaises(ValueError):
            self.f.trace()
            self.g.trace()

    @unittest.SkipTest
    def test_determinant(self):
        pass

    @unittest.SkipTest
    def test_sub_matrix(self):
        pass

    def test_is_square(self):
        self.assertEqual(self.a.is_square(), True)
        self.assertEqual(self.b.is_square(), True)
        self.assertEqual(self.c.is_square(), True)
        self.assertEqual(self.d.is_square(), True)
        self.assertEqual(self.e.is_square(), True)
        self.assertEqual(self.f.is_square(), False)
        self.assertEqual(self.g.is_square(), False)
        self.assertEqual(self.h.is_square(), True)
        self.assertEqual(self.i.is_square(), True)

    @unittest.SkipTest
    def test_is_symmetric(self):
        pass
