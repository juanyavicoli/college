// Calculate the minimum and maximum of three
// user-entered values and display them.

// I will assume that values are between -10000
// and 10000

program minmax;

var
    first: real;
    second: real;
    third: real;

    minimum: real = 10000;
    maximum: real = -10000;


function max(a, b: real): real;

begin

    if a >= b then
        max := a
    else
        max := b;

end;

function min(a, b: real): real;

begin

    if a <= b then
        min := a
    else
        min := b;

end;

begin

    ReadLn(first);
    ReadLn(second);
    ReadLn(third);

    if first >= second then
        if first >= third then
            begin
                maximum := first;

                if second <= third then
                    minimum := second
                else
                    minimum := third
            end
        else
            if second >= third then
                begin
                    maximum := second;
                    minimum := third;
                end
            else
                begin
                    maximum := third;
                    minimum := second;
                end
    else
        if second >= third then
            begin
                maximum := second;

                if first <= third then
                    minimum := first
                else
                    minimum := third
            end
        else
            begin
                maximum := third;
                minimum := first;
            end;

    WriteLn('Maximum:');
    WriteLn(maximum);

    WriteLn('Minimum:');
    WriteLn(minimum);

    // Now we make use of our functions.
    maximum := max(max(first, second), third);
    minimum := min(min(first, second), third);

    WriteLn('Function maximum:');
    WriteLn(maximum);

    WriteLn('Function minimum:');
    WriteLn(minimum);

end.

