{$MODE DELPHI}
program WordSearch;

uses
  SysUtils, Classes; // Include the necessary unit


const
  TargetWord = 'XMAS';
  Directions: array[1..8, 1..2] of Integer = (
    (0, 1),   // Right
    (1, 0),   // Down
    (1, 1),   // Diagonal-right-down
    (1, -1),  // Diagonal-left-down
    (0, -1),  // Left
    (-1, 0),  // Up
    (-1, -1), // Diagonal-left-up
    (-1, 1)   // Diagonal-right-up
  );

var
  Grid: array of array of Char;
  Rows, Cols: Integer;

function CheckWord(x, y, dx, dy: Integer): Boolean;
var
  i, nx, ny: Integer;
begin
  for i := 0 to Length(TargetWord) - 1 do
  begin
    nx := x + i * dx;
    ny := y + i * dy;
    if (nx < 0) or (ny < 0) or (nx >= Rows) or (ny >= Cols) or (Grid[nx][ny] <> TargetWord[i + 1]) then
    begin
      Exit(False);
    end;
  end;
  Exit(True);
end;

function CountOccurrences: Integer;
var
  r, c, d: Integer;
  dx, dy: Integer;
begin
  Result := 0;
  for r := 0 to Rows - 1 do
  begin
    for c := 0 to Cols - 1 do
    begin
      for d := 1 to 8 do
      begin
        dx := Directions[d][1];
        dy := Directions[d][2];
        if CheckWord(r, c, dx, dy) then
          Inc(Result);
      end;
    end;
  end;
end;

procedure ReadGridFromFile(const FilePath: String);
var
  FileContent: TStringList;
  i, j: Integer;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FilePath);
    Rows := FileContent.Count;
    Cols := Length(FileContent[0]);
    SetLength(Grid, Rows, Cols);

    for i := 0 to Rows - 1 do
    begin
      for j := 0 to Cols - 1 do
      begin
        Grid[i][j] := FileContent[i][j + 1];
      end;
    end;
  finally
    FileContent.Free;
  end;
end;

var
  FilePath: String;
  Count: Integer;
begin
  Write('Enter the file path: ');
  ReadLn(FilePath);

  try
    ReadGridFromFile(FilePath);
    Count := CountOccurrences;
    WriteLn('Occurrences of "', TargetWord, '": ', Count);
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
