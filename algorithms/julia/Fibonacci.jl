using Test


# NOTE: I felt obliged to add the non-negativity
# conditional instead of specifying an unsigned
# type for n; doing it required: converting all
# arguments before passing them to the function
# to unsigned and converting the right hand side
# of the comparison at the tests to unsigned, too.
function recursive_fibonacci(n)
	if n < 0
		throw(DomainError(n, "argument must be nonnegative"))
	elseif n < 2
		n
	else
		recursive_fibonacci(n - 1) + recursive_fibonacci(n - 2)
	end
end


@test recursive_fibonacci(0) == 0
@test recursive_fibonacci(1) == 1
@test recursive_fibonacci(2) == 1
@test recursive_fibonacci(3) == 2
@test recursive_fibonacci(4) == 3
@test recursive_fibonacci(5) == 5
@test recursive_fibonacci(6) == 8
@test recursive_fibonacci(7) == 13
@test recursive_fibonacci(8) == 21

