Imports System.Activities
Imports System.ComponentModel

Public Class SudokuSolver
    Inherits CodeActivity

    '<RequiredArgument>
    <Category("Input")>
    Public Property GridDictionary As InArgument(Of Dictionary(Of String, String))

    <Category("Output")>
    Public Property Result As OutArgument(Of Dictionary(Of String, String))
    Protected Overrides Sub Execute(context As CodeActivityContext)
        Dim g = New Grid() ' http://www.websudoku.com/?level=1&set_id=8663613710

        Dim dict = GridDictionary.Get(context)
        Dim useBackdoor As Boolean = True

        For Each kvp As KeyValuePair(Of String, String) In dict
            g.SetKnownValue(kvp.Key, dict(kvp.Key), useBackdoor)
        Next

        Dim bf As New Brute_Force(g)
        Dim results As Integer
        Console.WriteLine("Brute Forcing...")
        Dim t1, t2, time As Double
        t1 = 0
        t1 = 0
        time = 0
        t1 = Microsoft.VisualBasic.DateAndTime.Timer
        results = bf.Run
        t2 = Microsoft.VisualBasic.DateAndTime.Timer
        time = t2 - t1

        Console.WriteLine("Solving finished...")

        Dim outDict = New Dictionary(Of String, String)

        'are all numbers now known:
        Dim allknown As Boolean = True 'prove wrong
        Dim x As Integer = 0
        Dim y As Integer = 0
        For i As Integer = 0 To 80
            If g.KnownValue(i) = 0 Then allknown = False

            outDict.Add(x.ToString + y.ToString, g.KnownValue(i).ToString)
            y += 1
            If (y > 8) Then
                y = 0
                x += 1
            End If
        Next

        Result.Set(context, outDict)

        If results > 0 Then
            Console.WriteLine("Brute forced the solution in " & time.ToString & "s")
            Console.WriteLine("Maximium recursion depth: " & results.ToString)
        Else
            Console.WriteLine("Could not brute force a solution!")
        End If
    End Sub


End Class
