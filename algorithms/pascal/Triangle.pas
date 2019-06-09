// PROBLEM: Write a program that displays a triangle
// to the console using a procedure.

program Triangle;

// This procedure was proposed by our professor. It
// simply illustrates how to define and use a procedure.
procedure fixed_triangle;
    begin

        WriteLn('    *    ');
        WriteLn('  *   *  ');
        WriteLn(' *     * ');
        WriteLn('*       *');

    end;

procedure variable_triangle(size: Integer);
    var
        x: Integer;
        c: Integer;

    begin

        // This line is really important, otherwise, a garbage
        // value from the previous call will be taken.
        c := 0;

        while c < size do
            begin

                for x := 1 to size - c do
                    Write(' ');

                for x := size - c to size + c do
                    Write('*');

                WriteLn('');
                c := c + 1;

            end;

    end;

begin

    fixed_triangle();
    variable_triangle(3);
    variable_triangle(4);

end.

