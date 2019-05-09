#ifndef MATRIX_HPP_INCLUDED
#define MATRIX_HPP_INCLUDED

#include <stdexcept>
#include <type_traits>
#include <vector>

class Matrix
{
    public:
        class AsymmetryError : public std::runtime_error
        {
            public:
                AsymmetryError() : std::runtime_error("") {}
        };

        class NotInvertibleError : public std::runtime_error
        {
            public:
                NotInvertibleError() : std::runtime_error("") {}
        };

        class NotSquareError : public std::runtime_error
        {
            public:
                NotSquareError() : std::runtime_error("") {}
        };

        template <class T>
        using Vector        = std::vector<T>;
        using Value         = float;
        using Row           = Vector<Value>;

                            Matrix() = delete;
                            Matrix(const Matrix& other) = default;
                            Matrix(Matrix&& other) = default;

                            Matrix(Vector<Row> values);

        Matrix              operator+=(Matrix other);
        Matrix              operator-=(Matrix other);
        Matrix              operator*=(Matrix other);

        bool                operator==(Matrix other);
        bool                operator!=(Matrix other);

        bool                operator<(Matrix other) = delete;
        bool                operator>(Matrix other) = delete;
        bool                operator<=(Matrix other) = delete;
        bool                operator>=(Matrix other) = delete;

        unsigned int        rows();
        unsigned int        cols();

        Matrix              inverse();
        Matrix              transpose();
        float               trace();
        float               determinant();

        Matrix              submatrix(Vector<unsigned int> rows, Vector<unsigned int> cols);

        bool                is_square();
        bool                is_symmetric();
        bool                is_lower_triangular();
        bool                is_upper_triangular();

        static Matrix       identity(unsigned int size);
    private:
        Vector<Row>         m_values;
};

Matrix operator+(Matrix lhs, Matrix rhs);
Matrix operator-(Matrix lhs, Matrix rhs);
Matrix operator*(Matrix lhs, Matrix rhs);

#endif // MATRIX_HPP_INCLUDED
