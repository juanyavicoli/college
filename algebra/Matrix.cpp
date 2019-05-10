#include "Matrix.hpp"

Matrix::Matrix(Matrix::Vector<Matrix::Row> values)
{
    if (values.size() == 0)
        throw std::invalid_argument("");

    unsigned int first_row_size = values[0].size();

    for (const auto& row : values)
    {
        if (row.size() == 0)
            throw std::invalid_argument("");

        if (row.size() != first_row_size)
            throw Matrix::AsymmetryError();
    }

    m_values = values;
    m_rows = values.size();
    m_cols = first_row_size;
}

Matrix
Matrix::operator+=(Matrix other)
{
    if (rows() != other.rows() || cols() != other.cols())
        throw Matrix::AsymmetryError();

    Matrix::Row                     current_row;
    Matrix::Vector<Matrix::Row>     resultant_values;

    for (auto y = 0; y < rows(); ++y)
    {
        for (auto x = 0; x < cols(); ++x)
        {
            current_row.push_back(m_values[y][x] + other.m_values[y][x]);
        }

        resultant_values.push_back(current_row);
        current_row.clear();
    }

    m_values = resultant_values;

    return *this;
}

Matrix
Matrix::operator-=(Matrix other)
{
    if (rows() != other.rows() || cols() != other.cols())
        throw Matrix::AsymmetryError();

    Matrix::Row                     current_row;
    Matrix::Vector<Matrix::Row>     resultant_values;

    for (auto y = 0; y < rows(); ++y)
    {
        for (auto x = 0; x < cols(); ++x)
        {
            current_row.push_back(m_values[y][x] - other.m_values[y][x]);
        }

        resultant_values.push_back(current_row);
        current_row.clear();
    }
    
    m_values = resultant_values;

    return *this;
}

Matrix
Matrix::operator*=(Matrix other)
{
    if (cols() != other.rows())
        throw Matrix::AsymmetryError();

    Matrix::Value                   current_value = 0;
    Matrix::Row                     current_row;
    Matrix::Vector<Matrix::Row>     resultant_values;

    for (auto y = 0; y < rows(); ++y)
    {
        for (auto z = 0; z < other.cols(); ++z)
        {
            for (auto x = 0; x < cols(); ++x)
            {
                current_value += m_values[y][x] * other.m_values[x][z];
            }

            current_row.push_back(current_value);
            current_value = 0;
        }

        resultant_values.push_back(current_row);
        current_row.clear();
    }

    m_values = resultant_values;

    return *this;
}

bool
Matrix::operator==(Matrix other)
{
    return m_values == other.m_values;
}

bool
Matrix::operator!=(Matrix other)
{
    return !(*this == other);
}

unsigned int
Matrix::rows()
{
    return m_rows;
}

unsigned int
Matrix::cols()
{
    return m_cols;
}

Matrix
Matrix::inverse()
{

}

Matrix
Matrix::transpose()
{

}

float
Matrix::trace()
{
    if (not is_square())
        throw AsymmetryError();
        
    Matrix::Value   result = 0;
        
    // It doesn't matter if we use rows() or cols()
    // since the matrix must be square (and thus,
    // both functions should return the same value).
    for (auto i = 0; i < rows(); ++i)
    {
        result += m_values[i][i];
    }
    
    return result;
}

float
Matrix::determinant()
{

}

Matrix
Matrix::submatrix(Matrix::Vector<unsigned int> rows, Matrix::Vector<unsigned int> cols)
{

}

bool
Matrix::is_square()
{
    return cols() == rows();
}

bool
Matrix::is_symmetric()
{

}

bool
Matrix::is_lower_triangular()
{

}

bool
Matrix::is_upper_triangular()
{

}

Matrix
Matrix::identity(unsigned int size)
{

}

Matrix
operator+(Matrix lhs, Matrix rhs)
{
    lhs += rhs;
    return lhs;
}

Matrix
operator-(Matrix lhs, Matrix rhs)
{
    lhs -= rhs;
    return lhs;
}

Matrix
operator*(Matrix lhs, Matrix rhs)
{
    lhs *= rhs;
    return lhs;
}
