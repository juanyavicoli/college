program QuadraticEquationSolver;


var
    vQuadraticCoeff: real;
    vLinearCoeff: real;
    vConstantCoeff: real;
    vDiscriminant: real;
    vDenominator: real;

    vRoot1: real;
    vRoot2: real;


begin
    ReadLn(vQuadraticCoeff);
    ReadLn(vLinearCoeff);
    ReadLn(vConstantCoeff);

    vDiscriminant := sqr(vLinearCoeff) - 4 * vQuadraticCoeff * vConstantCoeff;

    if vDiscriminant > 0 then
        begin
            vDenominator := 2 * vQuadraticCoeff;

            vRoot1 := (-vLinearCoeff + sqrt(vDiscriminant)) / vDenominator;

            vRoot2 := (-vLinearCoeff - sqrt(vDiscriminant)) / vDenominator;

            WriteLn('Root 1: ', vRoot1, ' Root 2: ', vRoot2);
        end
    else
        WriteLn('The equation has irrational roots');
end.

