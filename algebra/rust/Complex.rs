#[derive(Debug, Clone, Copy, PartialEq)]
struct Complex {
    real: f64,
    im: f64,
}

impl std::ops::Add for Complex {
    type Output = Complex;

    fn add(self, rhs: Self) -> Self {
        Complex { real: self.real + rhs.real, im: self.im + rhs.im }
    }
}

impl std::ops::Sub for Complex {
    type Output = Complex;

    fn sub(self, rhs: Self) -> Self {
        Complex { real: self.real - rhs.real, im: self.im - rhs.im }
    }
}

#[test]
fn test_addition() {
    let a = Complex { real: 1.0, im: 1.0 };
    let b = Complex { real: 2.0, im: 1.0 };
    let c = Complex { real: -3.0, im: -1.0 };
    let d = Complex { real: 0.0, im: 0.0 };

    assert_eq!(a + b, Complex { real: 3.0, im: 2.0} );
    assert_eq!(a + c, Complex { real: -2.0, im: 0.0} );
    assert_eq!(b + c, Complex { real: -1.0, im: 0.0} );

    assert_eq!(a + b, b + a);
    assert_eq!(a + c, c + a);
    assert_eq!(b + c, c + b);

    assert_eq!(a + d, a);
    assert_eq!(b + d, b);
    assert_eq!(c + d, c);
}

#[test]
fn test_subtraction() {
    let a = Complex { real: 1.0, im: 1.0 };
    let b = Complex { real: 2.0, im: 1.0 };
    let c = Complex { real: -3.0, im: -1.0 };
    let d = Complex { real: 0.0, im: 0.0 };

    assert_eq!(a - b, Complex { real: -1.0, im: 0.0} );
    assert_eq!(a - c, Complex { real: 4.0, im: 2.0} );
    assert_eq!(b - c, Complex { real: 5.0, im: 2.0} );

    assert_eq!(a - d, a);
    assert_eq!(b - d, b);
    assert_eq!(c - d, c);
}

#[test]
fn test_equality() {
    let a = Complex { real: 1.0, im: 1.0 };
    let b = Complex { real: 2.0, im: 2.0 };
    let c = Complex { real: -1.0, im: 1.0 };
    let a2 = Complex { real: 1.0, im: 1.0 };
    let b2 = Complex { real: 2.0, im: 2.0 };
    let c2 = Complex { real: -1.0, im: 1.0 };

    assert_eq!(a, a);
    assert_eq!(a, a2);
    assert_eq!(a2, a);

    assert_eq!(b, b);
    assert_eq!(b, b2);
    assert_eq!(b2, b);

    assert_eq!(c, c);
    assert_eq!(c, c2);
    assert_eq!(c2, c);

    assert_ne!(a, b);
    assert_ne!(b, a);
    assert_ne!(a, c);
    assert_ne!(c, a);
    assert_ne!(b, c);
    assert_ne!(c, b);
}

