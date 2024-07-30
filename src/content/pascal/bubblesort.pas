program BubbleSort;

const
    SIZE = 10;

var
    arr: array[1..SIZE] of Integer;
    i, j, temp: Integer;

begin
    Randomize;

    // Fill the array with random integers
    for i := 1 to SIZE do
        arr[i] := Random(100);

    // Print the original array
    Write('Original array: ');
    for i := 1 to SIZE do
        Write(arr[i], ' ');
    WriteLn;

    // Bubble sort algorithm
    for i := 1 to SIZE - 1 do begin
        for j := 1 to SIZE - i do begin
            if arr[j] > arr[j + 1] then begin
                temp := arr[j];
                arr[j] := arr[j + 1];
                arr[j + 1] := temp;
            end;
        end;
    end;

    // Print the sorted array
    Write('Sorted array: ');
    for i := 1 to SIZE do
        Write(arr[i], ' ');
    WriteLn;
end.