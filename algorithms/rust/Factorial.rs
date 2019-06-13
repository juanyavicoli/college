fn recursive_factorial(n: i64) -> i64 {
    if n == 0 {
        1
    } else {
        n * recursive_factorial(n - 1)
    }
}

#[test]
fn test_recursive_factorial() {
    assert_eq!(recursive_factorial(0), 1);
    assert_eq!(recursive_factorial(1), 1);
    assert_eq!(recursive_factorial(2), 2);
    assert_eq!(recursive_factorial(3), 6);
    assert_eq!(recursive_factorial(4), 24);
    assert_eq!(recursive_factorial(5), 120);
    assert_eq!(recursive_factorial(6), 720);
}

