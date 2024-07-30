program Factor;

var
    number, i: integer;

begin
    Write('Enter a number: ');
    ReadLn(number);

    Write('Factors of ', number, ': ');
    for i := 2 to number do begin
        while number mod i = 0 do begin
            if number mod i = 0 then begin
                write(i, ' ');
                number := number div i;
            end;
        end;
    end;
    WriteLn;
end.