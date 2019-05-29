// DISCLAIMER: Use of functions, arrays and anything-not-shown-here
// isn't allowed. I would have used them otherwise.

// PROBLEM: Write a program that accepts an undeterminated amount
// of bills for up to three salesmen, that might be issued during
// the morning or the afternoon.

// Display the amount, the total price and the average price of the
// bills of each salesman.

// Display the number and price of the lowest bill, and the number
// of times that the highest one repeats.

// Display how many bills were issued in each shift by each salesman,
// and the salesman that produced the highest reports (bills).

// The original problem included a "item" variable, describing the
// object associated with the bill, however, it wasn't used so i
// decided to get rid of it.

// I will also assume that bills are in the range $-10000 to $10000

program sales;

var
    bill_number: integer = 0;
    bill_price: real = 0;
    bill_maximum_price: real = -10000;
    bill_maximum_amount: integer = 0;
    bill_date: string[8];

    shift: char;
    salesman: integer;

    keep_running: char;

    salesman_1_price_total: real = 0;
    salesman_2_price_total: real = 0;
    salesman_3_price_total: real = 0;

    salesman_1_price_average: real = 0;
    salesman_2_price_average: real = 0;
    salesman_3_price_average: real = 0;

    salesman_1_minimum_price: real = 10000;
    salesman_1_minimum_number: integer;
    salesman_1_minimum_date: string[8];

    salesman_2_minimum_price: real = 10000;
    salesman_2_minimum_number: integer;
    salesman_2_minimum_date: string[8];

    salesman_3_minimum_price: real = 10000;
    salesman_3_minimum_number: integer;
    salesman_3_minimum_date: string[8];

    salesman_1_amount_total: integer = 0;
    salesman_1_amount_morning: integer = 0;

    salesman_2_amount_total: integer = 0;
    salesman_2_amount_morning: integer = 0;

    salesman_3_amount_total: integer = 0;
    salesman_3_amount_morning: integer = 0;

    salesman_top: integer;

begin

    // TODO: Can i use a single Read() call
    // to fill all fields?
    ReadLn(bill_number);
    ReadLn(bill_price);
    ReadLn(bill_date);
    ReadLn(shift);
    ReadLn(salesman);
    ReadLn(keep_running);

    while keep_running = 'y' do
        begin

            case salesman of

                1:
                    begin
                        salesman_1_price_total := salesman_1_price_total + bill_price;

                        if bill_price < salesman_1_minimum_price then
                            begin
                                salesman_1_minimum_price := bill_price;
                                salesman_1_minimum_number := bill_number;
                                salesman_1_minimum_date := bill_date;
                            end;

                        if shift = 'm' then
                            salesman_1_amount_morning := salesman_1_amount_morning + 1;

                        salesman_1_amount_total := salesman_1_amount_total + 1;
                    end;

                2:
                    begin
                        salesman_2_price_total := salesman_2_price_total + bill_price;

                        if bill_price < salesman_2_minimum_price then
                            begin
                                salesman_2_minimum_price := bill_price;
                                salesman_2_minimum_number := bill_number;
                                salesman_2_minimum_date := bill_date;
                            end;

                        if shift = 'm' then
                            salesman_2_amount_morning := salesman_2_amount_morning + 1;

                        salesman_2_amount_total := salesman_2_amount_total + 1;
                    end;

                3:
                    begin
                        salesman_3_price_total := salesman_3_price_total + bill_price;

                        if bill_price < salesman_3_minimum_price then
                            begin
                                salesman_3_minimum_price := bill_price;
                                salesman_3_minimum_number := bill_number;
                                salesman_3_minimum_date := bill_date;
                            end;

                        if shift = 'm' then
                            salesman_3_amount_morning := salesman_3_amount_morning + 1;

                        salesman_3_amount_total := salesman_3_amount_total + 1;
                    end;

            end; // case shift of

            ReadLn(keep_running);

        end; // keep_running = 'y'

    // Calculate the highest bill overall and store its
    // date and number.

    if bill_price > bill_maximum_price then
        begin
            bill_maximum_price := bill_price;
            bill_maximum_amount := 1;
        end
    else
        if bill_price = bill_maximum_price then
            bill_maximum_amount := bill_maximum_amount + 1;

    // Calculate the salesman with the highest bill.

    if salesman_1_amount_total >= salesman_2_amount_total then
        if salesman_1_amount_total >= salesman_3_amount_total then
            salesman_top := 1
        else
            if salesman_2_amount_total >= salesman_2_amount_total then
                salesman_top := 2
            else
                salesman_top := 3
    else
        if salesman_2_amount_total >= salesman_3_amount_total then
            salesman_top := 2
        else
            salesman_top := 3;

    // Calculate averages.

    salesman_1_price_average := salesman_1_price_total / salesman_1_amount_total;
    salesman_2_price_average := salesman_2_price_total / salesman_2_amount_total;
    salesman_3_price_average := salesman_3_price_total / salesman_3_amount_total;

    // Display all data.

    WriteLn('Top sales:');
    WriteLn(salesman_top);

    WriteLn('Total sales price of salesman 1:');
    WriteLn(salesman_1_price_total);

    WriteLn('Total sales made by salesman 1:');
    WriteLn(salesman_1_amount_total);

    WriteLn('Morning sales made by salesman 1:');
    WriteLn(salesman_1_amount_morning);

    WriteLn('Afternoon sales made by salesman 1:');
    WriteLn(salesman_1_amount_total - salesman_1_amount_morning);

    WriteLn('Average sales of salesman 1:');
    WriteLn(salesman_1_price_average);

    WriteLn('Lowest bill number of salesman 1:');
    WriteLn(salesman_1_minimum_number);

    WriteLn('Lowest bill date of salesman 1:');
    WriteLn(salesman_1_minimum_date);

    // And everything else...
    // I feel kinda lazy to write it all. Functions and arrays
    // would ease things a lot.

end.

