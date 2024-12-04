Imports System.IO

Module XmasWordSearch

    ' Function to read the grid from the input file
    Function ReadGrid(filePath As String) As String()
        Dim lines As New List(Of String)
        Try
            For Each line As String In File.ReadLines(filePath)
                If Not String.IsNullOrWhiteSpace(line) Then
                    lines.Add(line.Trim())
                End If
            Next
        Catch ex As Exception
            Console.WriteLine($"Error reading file: {ex.Message}")
        End Try
        Return lines.ToArray()
    End Function

    ' Function to check if the word exists in a given direction
    Function CheckWord(grid() As String, x As Integer, y As Integer, dx As Integer, dy As Integer, word As String, rows As Integer, cols As Integer) As Boolean
        For i As Integer = 0 To word.Length - 1
            Dim nx = x + i * dx
            Dim ny = y + i * dy
            If nx < 0 OrElse ny < 0 OrElse nx >= rows OrElse ny >= cols OrElse grid(nx)(ny) <> word(i) Then
                Return False
            End If
        Next
        Return True
    End Function

    ' Count occurrences of the word "XMAS" in all directions
    Function CountXMAS(grid() As String) As Integer
        Dim targetWord As String = "XMAS"
        Dim directions(,) As Integer = {
            {0, 1}, {1, 0}, {1, 1}, {1, -1}, {0, -1}, {-1, 0}, {-1, -1}, {-1, 1}
        }
        Dim rows As Integer = grid.Length
        Dim cols As Integer = grid(0).Length
        Dim count As Integer = 0

        For r As Integer = 0 To rows - 1
            For c As Integer = 0 To cols - 1
                For i As Integer = 0 To directions.GetLength(0) - 1
                    Dim dx As Integer = directions(i, 0)
                    Dim dy As Integer = directions(i, 1)
                    If CheckWord(grid, r, c, dx, dy, targetWord, rows, cols) Then
                        count += 1
                    End If
                Next
            Next
        Next
        Return count
    End Function

    ' Count all X-MAS patterns in the grid
    Function CountAllXMASPatterns(grid() As String) As Integer
        Dim rows As Integer = grid.Length
        Dim cols As Integer = grid(0).Length
        Dim count As Integer = 0

        For r As Integer = 1 To rows - 2
            For c As Integer = 1 To cols - 2
                Dim center As Char = grid(r)(c)
                Dim topLeft As Char = grid(r - 1)(c - 1)
                Dim topRight As Char = grid(r - 1)(c + 1)
                Dim bottomLeft As Char = grid(r + 1)(c - 1)
                Dim bottomRight As Char = grid(r + 1)(c + 1)

                If center = "A"c Then
                    ' Pattern 1: M.S
                    If topLeft = "M"c AndAlso topRight = "S"c AndAlso bottomLeft = "M"c AndAlso bottomRight = "S"c Then count += 1
                    ' Pattern 2: S.M
                    If topLeft = "S"c AndAlso topRight = "M"c AndAlso bottomLeft = "S"c AndAlso bottomRight = "M"c Then count += 1
                    ' Pattern 3: M.M
                    If topLeft = "M"c AndAlso topRight = "M"c AndAlso bottomLeft = "S"c AndAlso bottomRight = "S"c Then count += 1
                    ' Pattern 4: S.S
                    If topLeft = "S"c AndAlso topRight = "S"c AndAlso bottomLeft = "M"c AndAlso bottomRight = "M"c Then count += 1
                End If
            Next
        Next
        Return count
    End Function

    Sub Main()
        Dim filePath As String = "/uploads/input.txt"
        Dim grid() As String = ReadGrid(filePath)

        If grid.Length = 0 Then
            Console.WriteLine("Error: Grid is empty or invalid.")
            Return
        End If

        Dim xmasCount As Integer = CountXMAS(grid)
        Console.WriteLine($"Count of XMAS: {xmasCount}")

        Dim xmasPatterns As Integer = CountAllXMASPatterns(grid)
        Console.WriteLine($"Total X-MAS patterns: {xmasPatterns}")
    End Sub

End Module
