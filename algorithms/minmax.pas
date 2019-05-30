// Calculate the minimum and maximum of three
// user-entered values and display them.

// I will assume that values are between -10000
// and 10000

program minmax;

var
    first: real;
    second: real;
    third: real;

    min: real = 10000;
    max: real = -10000;

begin

    ReadLn(first);
    ReadLn(second);
    ReadLn(third);

    if first >= second then
        if first >= third then
            begin
                max := first;

                if second <= third then
                    min := second
                else
                    min := third
            end
        else
            if second >= third then
                begin
                    max := second;
                    min := third;
                end
            else
                begin
                    max := third;
                    min := second;
                end
    else
        if second >= third then
            begin
                max := second;

                if first <= third then
                    min := first
                else
                    min := third
            end
        else
            begin
                max := third;
                min := first;
            end;

    WriteLn('Maximum:');
    WriteLn(max);

    WriteLn('Minimum:');
    WriteLn(min);

end.

