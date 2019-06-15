fn factor_chain(values: &[i64]) -> bool {
    for i in (1..values.len()).rev() {
        if values[i] % values[i - 1] != 0 {
            return false
        }
    }

    return true
}

#[test]
fn test_factor_chain() {
    assert_eq!(factor_chain( &[1,2] ), true);
    assert_eq!(factor_chain( &[2,4,8] ), true);
    assert_eq!(factor_chain( &[12,24,48] ), true);
    assert_eq!(factor_chain( &[6,6,6] ), true);

    assert_eq!(factor_chain( &[13,25] ), false);
    assert_eq!(factor_chain( &[2,4,6] ), false);
    assert_eq!(factor_chain( &[1,2,3] ), false);
    assert_eq!(factor_chain( &[2,4,8,15] ), false);
}

