program Factorial;

function recursive_factorial(n: integer): integer;
begin
    // For simplicity, and since we weren't teached
    // to use exceptions yet, we'll assume the input
    // is always positive.
    if n = 0 then
        recursive_factorial := 1
    else
        recursive_factorial := n * recursive_factorial(n - 1);
end;

begin

    WriteLn(recursive_factorial(0));
    WriteLn(recursive_factorial(1));
    WriteLn(recursive_factorial(2));
    WriteLn(recursive_factorial(3));
    WriteLn(recursive_factorial(4));
    WriteLn(recursive_factorial(5));
    WriteLn(recursive_factorial(6));

end.

