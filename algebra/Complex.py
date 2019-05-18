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
        return self * Complex(other, 0)
        
    def __rmul__(self, other: float):
        return self * other
        
    def __matmul__(self, other):
        real = self.real() * other.real() - self.imaginary() * other.imaginary()
        imaginary = self.real() * other.imaginary() + self.imaginary() * other.real()
        
        return Complex(real, imaginary)
        
    def __eq__(self, other) -> boolean:
        return (self.real() == other.real()) and (self.imaginary == other.imaginary)
        
    def real(self):
        return self.__re
    
    def imaginary(self):
        return self.__im
    
    def conjugate(self):
        return Complex(self.real(), -self.imaginary())
