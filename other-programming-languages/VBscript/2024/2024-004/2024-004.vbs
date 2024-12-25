' Define the file path
Dim filePath
filePath = "C:\advent-of-code-002\input-files\2024\2024-004\input.txt"

' Define the target word
Dim targetWord
targetWord = "XMAS"

' Read the file and store the grid
Dim grid()
ReDim grid(0)
Dim fso, file, text, lines, rowCount, colCount
Set fso = CreateObject("Scripting.FileSystemObject")
Set file = fso.OpenTextFile(filePath, 1)
rowCount = 0

Do Until file.AtEndOfStream
    ReDim Preserve grid(rowCount)
    grid(rowCount) = file.ReadLine
    rowCount = rowCount + 1
Loop
file.Close

colCount = Len(grid(0))

' Define directions for moving in the grid
Dim directions(7)
directions(0) = Array(0, 1)   ' Right
directions(1) = Array(1, 0)   ' Down
directions(2) = Array(1, 1)   ' Diagonal-right-down
directions(3) = Array(1, -1)  ' Diagonal-left-down
directions(4) = Array(0, -1)  ' Left
directions(5) = Array(-1, 0)  ' Up
directions(6) = Array(-1, -1) ' Diagonal-left-up
directions(7) = Array(-1, 1)  ' Diagonal-right-up

' Function to check if a word exists in a given direction
Function CheckWord(x, y, dx, dy)
    Dim i, nx, ny
    For i = 0 To Len(targetWord) - 1
        nx = x + i * dx
        ny = y + i * dy
        If nx < 0 Or ny < 0 Or nx >= rowCount Or ny >= colCount Then
            CheckWord = False
            Exit Function
        End If
        If Mid(grid(nx), ny + 1, 1) <> Mid(targetWord, i + 1, 1) Then
            CheckWord = False
            Exit Function
        End If
    Next
    CheckWord = True
End Function

' Count all occurrences of the target word
Dim count, r, c, direction
count = 0

For r = 0 To rowCount - 1
    For c = 0 To colCount - 1
        For Each direction In directions
            If CheckWord(r, c, direction(0), direction(1)) Then
                count = count + 1
            End If
        Next
    Next
Next

WScript.Echo "Count of target word: " & count

' Function to count all X-MAS patterns
Function CountAllXMASPatterns()
    Dim patternCount, r, c, center, topLeft, topRight, bottomLeft, bottomRight
    patternCount = 0
    For r = 1 To rowCount - 2
        For c = 1 To colCount - 2
            center = Mid(grid(r), c + 1, 1)
            topLeft = Mid(grid(r - 1), c, 1)
            topRight = Mid(grid(r - 1), c + 2, 1)
            bottomLeft = Mid(grid(r + 1), c, 1)
            bottomRight = Mid(grid(r + 1), c + 2, 1)

            If center = "A" Then
                If (topLeft = "M" And topRight = "S" And bottomLeft = "M" And bottomRight = "S") _
                Or (topLeft = "S" And topRight = "M" And bottomLeft = "S" And bottomRight = "M") _
                Or (topLeft = "M" And topRight = "M" And bottomLeft = "S" And bottomRight = "S") _
                Or (topLeft = "S" And topRight = "S" And bottomLeft = "M" And bottomRight = "M") Then
                    patternCount = patternCount + 1
                End If
            End If
        Next
    Next
    CountAllXMASPatterns = patternCount
End Function

Dim totalXmasPatterns
totalXmasPatterns = CountAllXMASPatterns()
WScript.Echo "Total X-MAS patterns: " & totalXmasPatterns
