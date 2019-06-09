// This exercise was written in class, previous to an introduction
// to functions. I don't own this code, it belongs to my professor.

program Fibonacci;

var
    n, c, accumulated : integer;
    x, y : integer;

begin
    writeln('Hello, world!');
    readln(n);

    x := 1;
    y := 1;

    while c < n do
        begin
            c := c + 1;

            if c <= 2 then
                accumulated := x
            else
                begin
                    accumulated := x + y;
                    x := y;
                    y := accumulated;
                end;

            // Display the Cth fibonacci value.
            writeln(accumulated);
        end;
end.

