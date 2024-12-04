{$MODE DELPHI}

program XmasPatternCounter;

uses
  SysUtils, Classes;

var
  Grid: array of array of Char;
  Rows, Cols: Integer;

function CountAllXmasPatterns: Integer;
var
  r, c: Integer;
  Center, TopLeft, TopRight, BottomLeft, BottomRight: Char;
begin
  Result := 0;

  // Traverse the grid, ensuring bounds for a 3x3 X-MAS pattern
  for r := 1 to Rows - 2 do
  begin
    for c := 1 to Cols - 2 do
    begin
      Center := Grid[r][c];
      TopLeft := Grid[r - 1][c - 1];
      TopRight := Grid[r - 1][c + 1];
      BottomLeft := Grid[r + 1][c - 1];
      BottomRight := Grid[r + 1][c + 1];

      // Check all valid X-MAS configurations
      if Center = 'A' then
      begin
        // Pattern 1: M.S
        if (TopLeft = 'M') and (TopRight = 'S') and (BottomLeft = 'M') and (BottomRight = 'S') then
          Inc(Result)
        // Pattern 2: S.M
        else if (TopLeft = 'S') and (TopRight = 'M') and (BottomLeft = 'S') and (BottomRight = 'M') then
          Inc(Result)
        // Pattern 3: M.M
        else if (TopLeft = 'M') and (TopRight = 'M') and (BottomLeft = 'S') and (BottomRight = 'S') then
          Inc(Result)
        // Pattern 4: S.S
        else if (TopLeft = 'S') and (TopRight = 'S') and (BottomLeft = 'M') and (BottomRight = 'M') then
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
      for j := 1 to Cols do
      begin
        Grid[i][j - 1] := FileContent[i][j];
      end;
    end;
  finally
    FileContent.Free;
  end;
end;

var
  FilePath: String;
  TotalXmasPatterns: Integer;
begin
  Write('Enter the file path: ');
  ReadLn(FilePath);

  try
    ReadGridFromFile(FilePath);
    TotalXmasPatterns := CountAllXmasPatterns;
    WriteLn('Total X-MAS patterns found: ', TotalXmasPatterns);
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
