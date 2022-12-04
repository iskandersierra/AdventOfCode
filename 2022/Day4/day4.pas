PROGRAM AoC_Day4_2022;

USES
  Classes, SysUtils;

VAR
    fileName: AnsiString;
    part: AnsiString;
    line: AnsiString;
    lines: TStringList;
    pair: TStringArray;
    from1, to1, from2, to2: LongInt;
    total: LongInt;

BEGIN
    fileName := paramStr(1);
    part := paramStr(2);

    lines := TStringList.Create;
    lines.LoadFromFile(fileName);
    total := 0;

    for line in lines do
    begin
        pair := line.Replace(',', '-', [rfReplaceAll]).Split('-');
        from1 := StrToInt(pair[0]);
        to1 := StrToInt(pair[1]);
        from2 := StrToInt(pair[2]);
        to2 := StrToInt(pair[3]);
        if part = '1' then
        begin
            if ((from1 <= from2) and (to2 <= to1)) or ((from2 <= from1) and (to1 <= to2)) then
            begin
                total := total + 1;
            end;
        end
        else if part = '2' then
        begin
            if not ((to1 < from2) or (to2 < from1)) then
            begin
                total := total + 1;
            end;
        end;
    end;

    writeln(total);
END.
