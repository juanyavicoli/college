program TypeAliasing;


type
    Character = char;
    String20 = string[20];
    String40 = string[40];


var
    vCharacter: Character;
    vString20: String20;
    vString40: String40;


begin

    vCharacter := 'a';
    vString20 := '01234567890123456789012345678901234567890123456789';
    vString40 := '01234567890123456789012345678901234567890123456789';

    WriteLn(vCharacter);
    WriteLn(vString20);
    WriteLn(vString40);

end.

