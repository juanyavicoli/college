import math


class Matrix:

    @staticmethod
    def identity(size: int):
        if size <= 0:
            raise ValueError("Size must be positive, non-zero.")

        resultant_rows = []
        current_row = [0] * size

        for i in range(size):
            resultant_rows.append(current_row.copy())
            resultant_rows[i][i] = 1

        return Matrix(resultant_rows)

    def __init__(self, data: list):
        if len(data) == 0:
            raise ValueError("Matrix must have at least 1 row and 1 column.")

        if len(data[0]) == 0:
            raise ValueError("Matrix must have at least 1 row and 1 column.")

        self.__data = data.copy()
        self.__rows = len(data)
        self.__cols = len(data[0])

        for y in range(self.rows()):
            if len(self.__data[y]) != self.cols():
                raise ValueError("Matrix isn't uniform.")

    def __add__(self, other):
        if self.rows() != other.rows() or self.cols() != other.cols():
            raise ValueError("Matrices must be of same dimensions.")

        resultant_rows = []
        current_row = []

        for y in range(self.rows()):
            for x in range(self.cols()):
                current_row.append(self.__data[y][x] + other.__data[y][x])

            resultant_rows.append(current_row.copy())
            current_row.clear()

        return Matrix(resultant_rows)

    def __sub__(self, other):
        if self.rows() != other.rows() or self.cols() != other.cols():
            raise ValueError("Matrices must be of same dimensions.")

        resultant_rows = []
        current_row = []

        for y in range(self.rows()):
            for x in range(self.cols()):
                current_row.append(self.__data[y][x] - other.__data[y][x])

            resultant_rows.append(current_row.copy())
            current_row.clear()

        return Matrix(resultant_rows)

    def __mul__(self, other: float):
        resultant_rows = []
        current_row = []

        for y in range(self.rows()):
            for x in range(self.cols()):
                current_row.append(other * self.__data[y][x])

            resultant_rows.append(current_row.copy())
            current_row.clear()

        return Matrix(resultant_rows)

    def __rmul__(self, other: float):
        return self * other

    def __matmul__(self, other):
        if self.cols() != other.rows():
            # @todo         Find a more descriptive message.
            raise ValueError("Matrices couldn't be multiplied.")

        resultant_rows = []
        current_row = []
        current_value = 0

        for y in range(self.rows()):
            for z in range(other.cols()):
                for x in range(self.cols()):
                    current_value += self.__data[y][x] * other.__data[x][z]

                current_row.append(current_value)
                current_value = 0

            resultant_rows.append(current_row.copy())
            current_row.clear()

        return Matrix(resultant_rows)

    def __eq__(self, other) -> bool:
        if self.rows() != other.rows() or self.cols() != other.cols():
            return False

        for y in range(self.rows()):
            for x in range(self.cols()):
                if not math.isclose(self.__data[y][x], other.__data[y][x]):
                    return False

        return True

    def __str__(self):
        return str(self.__data)

    def __repr__(self):
        return repr(self.__data)

    def data(self) -> list:
        return self.__data.copy()

    def rows(self) -> int:
        return self.__rows

    def cols(self) -> int:
        return self.__cols

    def inverse(self):
        if not self.is_square():
            raise ValueError("Matrix must be square.")

        original_determinant = self.determinant()

        if math.isclose(original_determinant, 0):
            raise ValueError("Determinant must be non-zero.")

        if self.rows() == 1:
            return self

        resultant_minors = []
        current_minors = []

        # For each element in the matrix, we remove its
        # row and column, and store the resultant sub matrix's
        # determinant in our list of minors.
        for y in range(self.rows()):
            for x in range(self.cols()):
                current_minors.append(self.sub_matrix([y], [x]).determinant())

            resultant_minors.append(current_minors.copy())
            current_minors.clear()

        alternator = 0

        # The matrix of minors (should) have the same size
        # than this one has.
        for y in range(self.rows()):
            if y % 2 == 0:
                alternator = 1
            else:
                alternator = -1

            for x in range(self.cols()):
                resultant_minors[y][x] *= alternator
                alternator *= -1

        # Generate the adjugate, which is the transpose of
        # the matrix of minors.
        resultant_matrix = Matrix(resultant_minors)
        resultant_matrix = resultant_matrix.transpose()

        resultant_matrix *= 1 / original_determinant

        return resultant_matrix

    def transpose(self):
        resultant_rows = []
        current_row = []

        for x in range(self.cols()):
            for y in range(self.rows()):
                current_row.append(self.__data[y][x])

            resultant_rows.append(current_row.copy())
            current_row.clear()

        return Matrix(resultant_rows)

    def trace(self) -> float:
        if not self.is_square():
            raise ValueError("Matrix must be square.")

        result = 0

        for i in range(self.rows()):
            result += self.__data[i][i]

        return result

    def rank(self) -> int:
        size = min(self.rows(), self.cols())

        keep_rows = []
        keep_cols = []

        while size > 0:
            for y in range(self.rows() - size + 1):
                for z in range(y, y + size):
                    keep_rows.append(z)

                for x in range(self.cols() - size + 1):
                    for w in range(x, x + size):
                        keep_cols.append(w)

                    inner_matrix = self.inner_matrix(keep_rows, keep_cols)

                    if inner_matrix.determinant() != 0:
                        return size

                keep_cols.clear()

            keep_rows.clear()

            size -= 1

        return size

    def determinant(self) -> float:
        if not self.is_square():
            raise ValueError("Matrix must be square.")

        result = 0

        if self.rows() > 1:
            alternator = 1

            for x in range(self.cols()):
                # Remove the first row and the current column.
                sub_matrix = self.sub_matrix([0], [x])

                result += alternator * self.__data[0][x] * sub_matrix.determinant()
                alternator *= -1
        else:
            result = self.__data[0][0]

        return result

    def inner_matrix(self, keep_rows: iter, keep_cols: iter):
        keep_rows = sorted(keep_rows)
        keep_cols = sorted(keep_cols)

        resultant_rows = []
        data = self.data()

        for y in range(self.rows()):
            if y in keep_rows:
                resultant_rows.append(data[y])

        for x in reversed(range(self.cols())):
            if x not in keep_cols:
                for row in resultant_rows:
                    del row[x]

        return Matrix(resultant_rows)

    def sub_matrix(self, remove_rows: list, remove_cols: list):
        resultant_rows = []
        current_row = []

        for y in range(self.rows()):
            if y not in remove_rows:
                for x in range(self.cols()):
                    if x not in remove_cols:
                        current_row.append(self.__data[y][x])

                if len(current_row) != 0:
                    resultant_rows.append(current_row.copy())
                    current_row.clear()

        return Matrix(resultant_rows)

    def super_matrix(self, append_rows: int, append_cols: int):
        if append_rows < 0 or append_cols < 0:
            raise ValueError("Parameters must be positive or zero.")

        resultant_rows = []
        current_row = []
        zeroed_row = [0] * (self.cols() + append_cols)

        for y in range(self.rows()):
            for x in range(self.cols()):
                current_row.append(self.__data[y][x])

            for x in range(self.cols(), self.cols() + append_cols):
                current_row.append(0)

            resultant_rows.append(current_row.copy())
            current_row.clear()

        for y in range(self.rows(), self.rows() + append_rows):
            resultant_rows.append(zeroed_row.copy())

        return Matrix(resultant_rows)

    def is_square(self) -> bool:
        if self.rows() == self.cols():
            return True
        else:
            return False

    def is_symmetric(self) -> bool:
        if self.transpose() == self:
            return True
        else:
            return False
