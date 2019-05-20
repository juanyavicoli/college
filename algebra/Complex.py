class Complex:
    
    def __init__(self, re: float, im: float):
        self.__re = re
        self.__im = im
        
    def __add__(self, other):
        real = self.real() + other.real()
        imaginary = self.imaginary() + other.imaginary()
        
        return Complex(real, imaginary)
        
    def __sub__(self, other):
        real = self.real() - other.real()
        imaginary = self.imaginary() - other.imaginary()
        
        return Complex(real, imaginary)
        
    def __mul__(self, other: float):
        return self @ Complex(other, 0)
        
    def __rmul__(self, other: float):
        return self * other
        
    def __matmul__(self, other):
        real = self.real() * other.real() - self.imaginary() * other.imaginary()
        imaginary = self.real() * other.imaginary() + self.imaginary() * other.real()
        
        return Complex(real, imaginary)
        
    def __eq__(self, other) -> bool:
        if self.real() != other.real():
            return False
            
        if self.imaginary() != other.imaginary():
            return False
            
        return True
        
    def real(self):
        return self.__re
    
    def imaginary(self):
        return self.__im
    
    def conjugate(self):
        return Complex(self.real(), -self.imaginary())
        
    def reciprocal(self):
        if self.is_zero():
            raise ValueError("Complex is zero")
            
        sum_of_squares = self.real() ** 2 + self.imaginary() ** 2
        
        return Complex(self.real() / sum_of_squares, -self.imaginary() / sum_of_squares)
            
    def is_zero(self):
        return math.isclose(self.real() ** 2 + self.imaginary() ** 2, 0)
            
    def is_real(self):
        return math.isclose(self.imaginary(), 0)
        
    def is_pure(self):
        return math.isclose(self.real(), 0)

