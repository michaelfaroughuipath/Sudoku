Public Class Brute_Force

    'Based on this C code http://www.users.waitrose.com/~nihilist/sudoku.html
    'original source is in brute.txt
    '" I'm not placing any GNU-type restrictions on this code. It's yours for the price of the bits."
    'Thanks therefore to original author. 

    'Dim frm1 As Form1
    Dim grd As Grid   
    Dim max_rec_depth As Integer = 0
    Dim cur_rec_depth As Integer = 0
    Dim sudoku(9, 9, 11) As Integer

    Public Sub New(ByRef g As Grid)
        grd = g
        initialise(grd)
    End Sub

    Public Function Run() As Integer
        Dim result As Boolean
        result = attempt_to_solve()
        If result = True Then
            overwriteGridWithSolution()
        Else
            Console.WriteLine("failed")
            max_rec_depth = 0   'indicate it failed
        End If
        Return max_rec_depth
    End Function

    Private Sub initialise(ByVal g As Grid)
        'gets the known values from the grid and puts them where the brute forcer wants them.
        Dim i, j, k, x As Integer
        Dim equivalent_grid_square As Integer = 0
        For i = 0 To (8)
            For j = 0 To (8)
                For k = 0 To (8)
                    sudoku(i, j, k) = 1
                Next
                sudoku(i, j, 9) = 9
                equivalent_grid_square = (i * 9) + j
                x = grd.KnownValue(equivalent_grid_square)
                If x > 0 Then
                    For k = 0 To (8)
                        sudoku(i, j, k) = 0
                    Next
                    sudoku(i, j, x - 1) = 1
                    sudoku(i, j, 9) = 1
                    sudoku(i, j, 10) = x
                End If
            Next
        Next
    End Sub

    Private Function attempt_to_solve() As Boolean
        Dim num_known As Integer
        Dim last_num_known As Integer
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim saved_entry(11) As Integer
        Dim saved_sudoku(9, 9, 11) As Integer
        Dim done As Integer
        cur_rec_depth += 1
        If (max_rec_depth < cur_rec_depth) Then
            max_rec_depth = cur_rec_depth
        End If
        num_known = number_known()
        While (num_known > last_num_known) And (num_known < 81)
            last_num_known = num_known
            For i = 0 To 8
                For j = 0 To 8
                    If sudoku(i, j, 9) > 1 Then
                        proc_row(i, j)
                        proc_col(i, j)
                        proc_block(i, j)
                    End If
                Next
            Next
            num_known = number_known()
        End While

        If do_check() = False Then
            cur_rec_depth -= 1
            Return False
        End If

        If (num_known = 81) Then
            cur_rec_depth -= 1
            Return True
        End If

        done = 0
        For i = 0 To 8
            For j = 0 To 8
                If sudoku(i, j, 9) > 1 Then
                    done = 1
                    Exit For
                End If
            Next
            If done = 1 Then
                Exit For
            End If
        Next

        Dim lenf As Integer = sudoku.GetLength(2)
        ReDim saved_entry(lenf)
        For u As Integer = 0 To lenf - 1
            saved_entry(u) = sudoku(i, j, u)
        Next

        For l As Integer = 0 To 8
            If saved_entry(l) = 1 Then
                For a As Integer = 0 To sudoku.GetLength(0) - 1
                    For b As Integer = 0 To sudoku.GetLength(1) - 1
                        For c As Integer = 0 To sudoku.GetLength(2) - 1
                            saved_sudoku(a, b, c) = sudoku(a, b, c)
                        Next
                    Next
                Next
                For m As Integer = 0 To 8
                    sudoku(i, j, m) = 0
                Next
                sudoku(i, j, l) = 1
                sudoku(i, j, 9) = 1
                sudoku(i, j, 10) = l + 1
                Dim success As Boolean
                success = attempt_to_solve()
                If success = True Then
                    cur_rec_depth -= 1
                    Return success
                Else
                    For a As Integer = 0 To sudoku.GetLength(0) - 1
                        For b As Integer = 0 To sudoku.GetLength(1) - 1
                            For c As Integer = 0 To sudoku.GetLength(2) - 1
                                sudoku(a, b, c) = saved_sudoku(a, b, c)
                            Next
                        Next
                    Next
                End If

            End If
        Next
    End Function

    Private Function number_known() As Integer
        Dim n As Integer
        n = 0
        For i As Integer = 0 To 8
            For j As Integer = 0 To 8
                If sudoku(i, j, 9) = 1 Then
                    n += 1
                End If
            Next
        Next
        Return n
    End Function

    Private Sub proc_row(ByVal i As Integer, ByVal j As Integer)

        For k As Integer = 0 To (8)
            If (Not (j = k)) AndAlso (sudoku(i, k, 9) = 1) AndAlso (sudoku(i, j, 9) > 1) Then
                If ((sudoku(i, j, sudoku(i, k, 10) - 1)) = 1) Then
                    sudoku(i, j, sudoku(i, k, 10) - 1) = 0
                    Dim c As Integer = sudoku(i, j, 9)
                    sudoku(i, j, 9) = c - 1
                    If sudoku(i, j, 9) = 1 Then
                        sudoku(i, j, 10) = unique_value(i, j)
                    End If
                End If
            End If
        Next
    End Sub

    Private Sub proc_col(ByVal i As Integer, ByVal j As Integer)

        For k As Integer = 0 To 8
            If (Not (i = k)) AndAlso (sudoku(k, j, 9) = 1) AndAlso (sudoku(i, j, 9) > 1) Then
                If sudoku(i, j, sudoku(k, j, 10) - 1) = 1 Then
                    sudoku(i, j, sudoku(k, j, 10) - 1) = 0
                    Dim c As Integer = sudoku(i, j, 9)
                    sudoku(i, j, 9) = c - 1
                    If sudoku(i, j, 9) = 1 Then
                        sudoku(i, j, 10) = unique_value(i, j)
                    End If
                End If
            End If
        Next
    End Sub

    Private Sub proc_block(ByVal i As Integer, ByVal j As Integer)
        Dim x, y As Integer
        x = (i \ 3) * 3
        y = (j \ 3) * 3
        For k As Integer = x To x + 2
            For l As Integer = y To y + 2
                If ((Not (k = i)) Or (Not (l = j))) AndAlso (sudoku(k, l, 9) = 1) Then
                    If (sudoku(i, j, sudoku(k, l, 10) - 1) = 1) Then
                        sudoku(i, j, sudoku(k, l, 10) - 1) = 0
                        Dim c As Integer = sudoku(i, j, 9)
                        sudoku(i, j, 9) = c - 1
                        If sudoku(i, j, 9) = 1 Then
                            sudoku(i, j, 10) = unique_value(i, j)
                        End If
                    End If
                End If
            Next
        Next

    End Sub

    Private Function unique_value(ByVal x As Integer, ByVal y As Integer) As Integer

        Dim output As Integer = 10

        For i As Integer = 0 To 8
            If sudoku(x, y, i) = 1 Then
                output = i + 1
                Exit For
            End If
        Next

        Return output
    End Function

    Private Function do_check() As Boolean
        Dim x, y As Integer
        For i As Integer = 0 To (8)
            For j As Integer = 0 To (8)
                If sudoku(i, j, 9) = 0 Then
                    Return False
                End If
                For k As Integer = 0 To (8)
                    If (sudoku(i, k, 9) = 1) AndAlso (sudoku(i, j, 9) = 1) _
                        AndAlso (Not (j = k)) AndAlso (sudoku(i, k, 10) = sudoku(i, j, 10)) Then
                        Return False
                    End If
                Next
                For k As Integer = 0 To (8)
                    If (sudoku(k, j, 9) = 1) AndAlso (sudoku(i, j, 9) = 1) _
                        AndAlso (Not (i = k)) AndAlso (sudoku(k, j, 10) = sudoku(i, j, 10)) Then
                        Return False
                    End If
                Next
                x = (i \ 3) * 3
                y = (j \ 3) * 3
                For k As Integer = x To x + 2
                    For l As Integer = y To y + 2
                        If (sudoku(k, l, 9) = 1) AndAlso _
                        (sudoku(i, j, 9) = 1) AndAlso _
                        ((Not (k = i)) Or (Not (l = j))) AndAlso _
                        (sudoku(k, l, 10) = sudoku(i, j, 10)) Then
                            Return False
                        End If
                    Next
                Next
            Next
        Next
        Return True
    End Function

    Private Sub overwriteGridWithSolution()

        For i As Integer = 0 To 8
            For j As Integer = 0 To 8
                grd.SetKnownValue((i * 9) + j, sudoku(i, j, 10))
            Next
        Next

    End Sub
End Class
