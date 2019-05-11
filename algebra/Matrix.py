class Matrix:

    def __init__(self, data: list):
        self.__data = data
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
                if self.__data[y][x] != other.__data[y][x]:
                    return False

        return True

    def __str__(self):
        return str(self.__data)

    def __repr__(self):
        return repr(self.__data)

    def rows(self) -> int:
        return self.__rows

    def cols(self) -> int:
        return self.__cols

    def inverse(self):
        pass

    def transpose(self):
        pass

    def trace(self) -> float:
        pass

    def determinant(self) -> float:
        pass

    def sub_matrix(self):
        pass

    def is_square(self) -> bool:
        pass

    def is_symmetric(self) -> bool:
        pass
