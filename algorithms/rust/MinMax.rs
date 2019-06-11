fn min(a: i64, b: i64) -> i64 {
	if a <= b {
		a
	}
	else {
		b
	}
}

fn max(a: i64, b: i64) -> i64 {
	if a >= b {
		a
	}
	else {
		b
	}
}

#[test]
fn test_min() {
	assert_eq!(min(10, 20), 10);
	assert_eq!(min(10, 10), 10);
	assert_eq!(min(-50, 1), -50);
	assert_eq!(min(-1, 0), -1);
}

#[test]
fn test_max() {
	assert_eq!(max(10, 20), 20);
	assert_eq!(max(10, 10), 10);
	assert_eq!(max(-50, 1), 1);
	assert_eq!(max(-1, 0), 0);
}

