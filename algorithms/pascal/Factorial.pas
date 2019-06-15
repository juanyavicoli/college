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


function while_loop_factorial(n: integer): integer;
var
    c: integer;
begin
    while_loop_factorial := 1;
    c := 1;

    while c < n do
    begin
        while_loop_factorial := while_loop_factorial * c;
        c := c + 1;
    end;
end;


function for_loop_factorial(n: integer): integer;
var
    c: integer;
begin
    for_loop_factorial := 1;

    for c := 1 to n do
        for_loop_factorial := for_loop_factorial * c;
end;


begin

    WriteLn(recursive_factorial(0));
    WriteLn(recursive_factorial(1));
    WriteLn(recursive_factorial(2));
    WriteLn(recursive_factorial(3));
    WriteLn(recursive_factorial(4));
    WriteLn(recursive_factorial(5));
    WriteLn(recursive_factorial(6));

    WriteLn(while_loop_factorial(0));
    WriteLn(while_loop_factorial(1));
    WriteLn(while_loop_factorial(2));
    WriteLn(while_loop_factorial(3));
    WriteLn(while_loop_factorial(4));
    WriteLn(while_loop_factorial(5));
    WriteLn(while_loop_factorial(6));

    WriteLn(for_loop_factorial(0));
    WriteLn(for_loop_factorial(1));
    WriteLn(for_loop_factorial(2));
    WriteLn(for_loop_factorial(3));
    WriteLn(for_loop_factorial(4));
    WriteLn(for_loop_factorial(5));
    WriteLn(for_loop_factorial(6));

end.

