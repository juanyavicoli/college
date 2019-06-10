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
        self.j = Matrix([[1, 2], [2, 3]])
        self.k = Matrix([[5, 6, 7], [6, 3, 2], [7, 2, 1]])

        self.assertEqual(Matrix.identity(1), Matrix([[1]]))
        self.assertEqual(Matrix.identity(2), Matrix([[1, 0], [0, 1]]))
        self.assertEqual(Matrix.identity(3), Matrix([[1, 0, 0], [0, 1, 0], [0, 0, 1]]))
        self.assertEqual(Matrix.identity(4), Matrix([[1, 0, 0, 0],
                                                     [0, 1, 0, 0],
                                                     [0, 0, 1, 0],
                                                     [0, 0, 0, 1]]))

        with self.assertRaises(ValueError):
            dummy = Matrix.identity(0)
            dummy = Matrix.identity(-1)
            dummy = Matrix.identity(-467)

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
        self.assertEqual(self.a * 5, Matrix([[5]]))
        self.assertEqual(self.b * 4, Matrix([[8]]))
        self.assertEqual(4 * self.b, Matrix([[8]]))
        self.assertEqual(2 * self.c, Matrix([[20]]))
        self.assertEqual(self.d * 10, Matrix([[10, 20], [30, 40]]))
        self.assertEqual(self.e * 4.2, Matrix([[42, 42], [42, 42]]))
        self.assertEqual(1.1 * self.f, Matrix([[13.2, 7.7, 4.4], [6.6, 8.8, 2.2]]))
        self.assertEqual(self.g * (-2.5), Matrix([[-60, 20, 0], [2.5, -5, -5]]))
        self.assertEqual(self.h * 0.5, Matrix([[0.5, 1, 1.5], [2, 2.5, 3], [3.5, 4, 4.5]]))
        self.assertEqual(self.i * 10, Matrix([[50, 30, 80], [60, 10, 10], [90, 50, 70]]))

    def test_matrix_multiplication(self):
        self.assertEqual(self.a @ self.b, Matrix([[2]]))
        self.assertEqual(self.b @ self.c, Matrix([[20]]))
        self.assertEqual(self.d @ self.e, Matrix([[30, 30], [70, 70]]))
        self.assertEqual(self.e @ self.d, Matrix([[40, 60], [40, 60]]))
        self.assertEqual(self.g @ self.h, Matrix([[-8, 8, 24], [21, 24, 27]]))
        self.assertEqual(self.g @ self.i, Matrix([[72, 64, 184], [25, 9, 8]]))
        self.assertEqual(self.h @ self.i, Matrix([[44, 20, 31], [104, 47, 79], [164, 74, 127]]))
        self.assertEqual(self.i @ self.h, Matrix([[73, 89, 105], [17, 25, 33], [78, 99, 120]]))

        with self.assertRaises(ValueError):
            dummy = self.a @ self.d
            dummy = self.a @ self.h
            dummy = self.f @ self.g
            dummy = self.g @ self.f

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

    def test_data(self):
        self.assertEqual(self.a.data(), [[1]])
        self.assertEqual(self.b.data(), [[2]])
        self.assertEqual(self.c.data(), [[10]])
        self.assertEqual(self.d.data(), [[1, 2], [3, 4]])
        self.assertEqual(self.e.data(), [[10, 10], [10, 10]])
        self.assertEqual(self.f.data(), [[12, 7, 4], [6, 8, 2]])
        self.assertEqual(self.g.data(), [[24, -8, 0], [-1, 2, 2]])
        self.assertEqual(self.h.data(), [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
        self.assertEqual(self.i.data(), [[5, 3, 8], [6, 1, 1], [9, 5, 7]])
        self.assertEqual(self.j.data(), [[1, 2], [2, 3]])
        self.assertEqual(self.k.data(), [[5, 6, 7], [6, 3, 2], [7, 2, 1]])

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

    def test_opposite(self):
        self.assertEqual(self.a.opposite(), Matrix([[-1]]))
        self.assertEqual(self.b.opposite(), Matrix([[-2]]))
        self.assertEqual(self.c.opposite(), Matrix([[-10]]))
        self.assertEqual(self.d.opposite(), Matrix([[-1, -2], [-3, -4]]))
        self.assertEqual(self.e.opposite(), Matrix([[-10, -10], [-10, -10]]))
        self.assertEqual(self.f.opposite(), Matrix([[-12, -7, -4], [-6, -8, -2]]))
        self.assertEqual(self.g.opposite(), Matrix([[-24, 8, 0], [1, -2, -2]]))
        self.assertEqual(self.h.opposite(), Matrix([[-1, -2, -3], [-4, -5, -6], [-7, -8, -9]]))
        self.assertEqual(self.i.opposite(), Matrix([[-5, -3, -8], [-6, -1, -1], [-9, -5, -7]]))
        self.assertEqual(self.j.opposite(), Matrix([[-1, -2], [-2, -3]]))
        self.assertEqual(self.k.opposite(), Matrix([[-5, -6, -7], [-6, -3, -2], [-7, -2, -1]]))

    def test_inverse(self):
        self.assertEqual(self.a.inverse(), Matrix([[1]]))
        self.assertEqual(self.b.inverse(), Matrix([[2]]))
        self.assertEqual(self.c.inverse(), Matrix([[10]]))
        self.assertEqual(self.d.inverse(), Matrix([[-2, 1], [3/2, -1/2]]))
        self.assertEqual(self.i.inverse(), Matrix([[2/79, 19/79, -5/79],
                                                   [-33/79, -37/79, 43/79],
                                                   [21/79, 2/79, -13/79]]))
        self.assertEqual(self.j.inverse(), Matrix([[-3, 2], [2, -1]]))
        self.assertEqual(self.k.inverse(), Matrix([[1/20, -2/5, 9/20],
                                                   [-2/5, 11/5, -8/5],
                                                   [9/20, -8/5, 21/20]]))

        with self.assertRaises(ValueError):
            dummy = self.e.inverse()
            dummy = self.f.inverse()
            dummy = self.g.inverse()
            dummy = self.h.inverse()

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

    def test_rank(self):
        self.assertEqual(self.a.rank(), 1)
        self.assertEqual(self.b.rank(), 1)
        self.assertEqual(self.c.rank(), 1)
        self.assertEqual(self.d.rank(), 2)
        self.assertEqual(self.e.rank(), 1)
        self.assertEqual(self.f.rank(), 2)
        self.assertEqual(self.g.rank(), 2)
        self.assertEqual(self.h.rank(), 2)
        self.assertEqual(self.i.rank(), 3)
        self.assertEqual(self.j.rank(), 2)
        self.assertEqual(self.k.rank(), 3)

    def test_determinant(self):
        self.assertEqual(self.a.determinant(), 1)
        self.assertEqual(self.b.determinant(), 2)
        self.assertEqual(self.c.determinant(), 10)
        self.assertEqual(self.d.determinant(), -2)
        self.assertEqual(self.e.determinant(), 0)
        self.assertEqual(self.h.determinant(), 0)
        self.assertEqual(self.i.determinant(), 79)

        with self.assertRaises(ValueError):
            dummy = self.f.determinant()
            dummy = self.g.determinant()

    def test_inner_matrix(self):
        self.assertEqual(self.a.inner_matrix([0], [0]), self.a)
        self.assertEqual(self.b.inner_matrix([0], [0]), self.b)
        self.assertEqual(self.c.inner_matrix([0], [0]), self.c)
        self.assertEqual(self.d.inner_matrix([0, 1], [0, 1]), self.d)
        self.assertEqual(self.e.inner_matrix([0, 1], [0, 1]), self.e)
        self.assertEqual(self.f.inner_matrix([0, 1], [0, 1, 2]), self.f)
        self.assertEqual(self.g.inner_matrix([0, 1], [0, 1, 2]), self.g)
        self.assertEqual(self.h.inner_matrix([0, 1, 2], [0, 1, 2]), self.h)
        self.assertEqual(self.i.inner_matrix([0, 1, 2], [0, 1, 2]), self.i)
        self.assertEqual(self.j.inner_matrix([0, 1], [0, 1]), self.j)
        self.assertEqual(self.k.inner_matrix([0, 1, 2], [0, 1, 2]), self.k)

        with self.assertRaises(ValueError):
            dummy = self.a.inner_matrix([], [])
            dummy = self.a.inner_matrix([1], [0])
            dummy = self.a.inner_matrix([0], [1])
            dummy = self.a.inner_matrix([1], [1])

    def test_sub_matrix(self):
        self.assertEqual(self.a.sub_matrix([], []), self.a)
        self.assertEqual(self.b.sub_matrix([], []), self.b)
        self.assertEqual(self.c.sub_matrix([], []), self.c)
        self.assertEqual(self.d.sub_matrix([], []), self.d)
        self.assertEqual(self.e.sub_matrix([], []), self.e)
        self.assertEqual(self.f.sub_matrix([], []), self.f)
        self.assertEqual(self.g.sub_matrix([], []), self.g)
        self.assertEqual(self.h.sub_matrix([], []), self.h)
        self.assertEqual(self.i.sub_matrix([], []), self.i)

        self.assertEqual(self.a.sub_matrix([10], [11, 99]), self.a)
        self.assertEqual(self.f.sub_matrix([23], [256]), self.f)
        self.assertEqual(self.g.sub_matrix([99], [999]), self.g)

        self.assertEqual(self.d.sub_matrix([], [0]), Matrix([[2], [4]]))
        self.assertEqual(self.d.sub_matrix([], [1]), Matrix([[1], [3]]))
        self.assertEqual(self.d.sub_matrix([1], []), Matrix([[1, 2]]))
        self.assertEqual(self.d.sub_matrix([0], [0]), Matrix([[4]]))

        self.assertEqual(self.e.sub_matrix([0], []), Matrix([[10, 10]]))
        self.assertEqual(self.e.sub_matrix([], [0]), Matrix([[10], [10]]))

        self.assertEqual(self.f.sub_matrix([0], []), Matrix([[6, 8, 2]]))
        self.assertEqual(self.f.sub_matrix([1], []), Matrix([[12, 7, 4]]))
        self.assertEqual(self.f.sub_matrix([], [2]), Matrix([[12, 7], [6, 8]]))
        self.assertEqual(self.f.sub_matrix([], [1]), Matrix([[12, 4], [6, 2]]))
        self.assertEqual(self.f.sub_matrix([], [0, 1]), Matrix([[4], [2]]))
        self.assertEqual(self.f.sub_matrix([], [1, 2]), Matrix([[12], [6]]))

        self.assertEqual(self.h.sub_matrix([2], []), Matrix([[1, 2, 3], [4, 5, 6]]))
        self.assertEqual(self.h.sub_matrix([2], [2]), Matrix([[1, 2], [4, 5]]))

        with self.assertRaises(ValueError):
            dummy = self.a.sub_matrix([], [0])
            dummy = self.a.sub_matrix([0], [])
            dummy = self.a.sub_matrix([0, 1, 10], [99, 15, 0, 6])

            dummy = self.d.sub_matrix([0, 1], [0, 1])
            dummy = self.d.sub_matrix([1, 0], [1, 0])

    def test_super_matrix(self):
        self.assertEqual(self.a.super_matrix(1, 0), Matrix([[1], [0]]))
        self.assertEqual(self.a.super_matrix(0, 1), Matrix([[1, 0]]))
        self.assertEqual(self.a.super_matrix(1, 1), Matrix([[1, 0], [0, 0]]))
        self.assertEqual(self.b.super_matrix(1, 0), Matrix([[2], [0]]))
        self.assertEqual(self.b.super_matrix(0, 1), Matrix([[2, 0]]))
        self.assertEqual(self.b.super_matrix(1, 1), Matrix([[2, 0], [0, 0]]))
        self.assertEqual(self.c.super_matrix(1, 0), Matrix([[10], [0]]))
        self.assertEqual(self.c.super_matrix(0, 1), Matrix([[10, 0]]))
        self.assertEqual(self.c.super_matrix(1, 1), Matrix([[10, 0], [0, 0]]))
        self.assertEqual(self.d.super_matrix(1, 0), Matrix([[1, 2], [3, 4], [0, 0]]))
        self.assertEqual(self.d.super_matrix(0, 1), Matrix([[1, 2, 0], [3, 4, 0]]))
        self.assertEqual(self.d.super_matrix(1, 1), Matrix([[1, 2, 0], [3, 4, 0], [0, 0, 0]]))

        with self.assertRaises(ValueError):
            dummy = self.a.super_matrix(-1, 0)
            dummy = self.b.super_matrix(-15, 15)
            dummy = self.c.super_matrix(20, -999)
            dummy = self.d.super_matrix(-45, -45)

    def test_duplicate(self):
        self.assertEqual(self.a.duplicate(), self.a)
        self.assertEqual(self.b.duplicate(), self.b)
        self.assertEqual(self.c.duplicate(), self.c)
        self.assertEqual(self.d.duplicate(), self.d)
        self.assertEqual(self.e.duplicate(), self.e)
        self.assertEqual(self.f.duplicate(), self.f)
        self.assertEqual(self.g.duplicate(), self.g)
        self.assertEqual(self.h.duplicate(), self.h)
        self.assertEqual(self.i.duplicate(), self.i)
        self.assertEqual(self.j.duplicate(), self.j)
        self.assertEqual(self.k.duplicate(), self.k)

        self.assertEqual(self.a.duplicate(), Matrix([[1]]))
        self.assertEqual(self.b.duplicate(), Matrix([[2]]))
        self.assertEqual(self.c.duplicate(), Matrix([[10]]))
        self.assertEqual(self.d.duplicate(), Matrix([[1, 2], [3, 4]]))
        self.assertEqual(self.e.duplicate(), Matrix([[10, 10], [10, 10]]))
        self.assertEqual(self.f.duplicate(), Matrix([[12, 7, 4], [6, 8, 2]]))
        self.assertEqual(self.g.duplicate(), Matrix([[24, -8, 0], [-1, 2, 2]]))
        self.assertEqual(self.h.duplicate(), Matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]]))
        self.assertEqual(self.i.duplicate(), Matrix([[5, 3, 8], [6, 1, 1], [9, 5, 7]]))
        self.assertEqual(self.j.duplicate(), Matrix([[1, 2], [2, 3]]))
        self.assertEqual(self.k.duplicate(), Matrix([[5, 6, 7], [6, 3, 2], [7, 2, 1]]))

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

    def test_is_symmetric(self):
        self.assertEqual(self.a.is_symmetric(), True)
        self.assertEqual(self.b.is_symmetric(), True)
        self.assertEqual(self.c.is_symmetric(), True)
        self.assertEqual(self.d.is_symmetric(), False)
        self.assertEqual(self.e.is_symmetric(), True)
        self.assertEqual(self.f.is_symmetric(), False)
        self.assertEqual(self.g.is_symmetric(), False)
        self.assertEqual(self.h.is_symmetric(), False)
        self.assertEqual(self.i.is_symmetric(), False)
        self.assertEqual(self.j.is_symmetric(), True)
        self.assertEqual(self.k.is_symmetric(), True)
