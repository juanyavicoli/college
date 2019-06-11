using Test


function min(a::Number, b::Number)
	if a <= b
		a
	else
		b
	end
end

function max(a::Number, b::Number)
	if a >= b
		a
	else
		b
	end
end


@test min(10, 20) == 10
@test min(10, 10) == 10
@test min(-50, 1) == -50
@test min(-1, 0) == -1

@test max(10, 20) == 20
@test max(10, 10) == 10
@test max(-50, 1) == 1
@test max(-1, 0) == 0

