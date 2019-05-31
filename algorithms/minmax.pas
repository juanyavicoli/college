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

end.

