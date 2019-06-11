fn recursive_fibonacci(n: u64) -> u64 {
    if n < 2 {
        n
    } else {
        recursive_fibonacci(n - 1) + recursive_fibonacci(n - 2)
    }
}

#[test]
fn test_recursive_fibonacci() {
    assert_eq!(recursive_fibonacci(0), 0);
    assert_eq!(recursive_fibonacci(1), 1);
    assert_eq!(recursive_fibonacci(2), 1);
    assert_eq!(recursive_fibonacci(3), 2);
    assert_eq!(recursive_fibonacci(4), 3);
    assert_eq!(recursive_fibonacci(5), 5);
    assert_eq!(recursive_fibonacci(6), 8);
    assert_eq!(recursive_fibonacci(7), 13);
    assert_eq!(recursive_fibonacci(8), 21);
}

