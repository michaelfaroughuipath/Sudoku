
Public Class Grid
    'Array to hold all squares.
    Private sq(80) As Integer
    Public FirstSquareInBlock() As Integer = {0, 3, 6, 27, 30, 33, 54, 57, 60}
    'So we have:
    ' 
    ' 0  1  2 |3  4  5 |6  7  8 
    ' 9  10 11|12 13 14|15 16 17
    ' 18 19 20|21 22 23|24 25 26
    ' --------------------------
    ' 27 28 29|30 31 32|33 34 35
    ' 36 37 38|39 40 41|42 43 44
    ' 45 46 47|48 49 50|51 52 53
    ' --------------------------
    ' 54 55 56|57 58 59|60 61 62
    ' 63 64 65|66 67 68|69 70 71
    ' 72 73 74|75 76 77|78 89 80
    ' 
    ' the index in the array (index) refers to the square number 0 - 80
    ' the integers stored in  the array
    ' will be storing a binary map of the numbers that are still valid
    ' in that particular sqaure:
    ' number: 1 2 3 4 5 6 7 8 9
    ' valid:  ? ? ? ? ? ? ? ? ?   (? = 1 or 0, true or false)
    ' so you could have 010100100 - convert this binary to an integer to store it in the array.

    Private certainValue(80) As Integer
    'simple array storing certain values

    Public Sub New()
        ' when grid is initialised make it so that all numbers are possible in all squares
        ResetAllSquares()
    End Sub

    Public Sub SetKnownValue(ByVal key As String, ByVal value As Integer, backDoor As Boolean)

        Dim key1 As Integer = Int32.Parse(key.Substring(0, 1))
        Dim key2 As Integer = Int32.Parse(key.Substring(1, 1))

        Dim index As Integer = (9 * key1) + key2

        sq(index) = CInt(2 ^ (value - 1)) ' ^ expression returns a double. square should be int
        Console.WriteLine("Key: " + key + " Key1: " + key1.ToString + " Key2: " + key2.ToString + " index: " + index.ToString + "Value: " + value.ToString)
        certainValue(index) = value

    End Sub


    Public Sub SetKnownValue(ByVal index As String, ByVal value As Integer)
        ' When the contents of a square are known for certain, e.g.
        ' when a number gets dragged onto a square, 
        ' change the possible values to reflect this.
        ' Here is the binary map for when the value is known:
        ' value is 1: store this binary: 000000001 = 1
        ' value is 2: store this binary: 000000010 = 2
        ' value is 3: store this binary: 000000100 = 4
        ' value is 4: store this binary: 000001000 = 8
        ' value is 5: store this binary: 000010000 = 16
        ' etc ... , the number to store is 2 ^ (value - 1)

        sq(index) = CInt(2 ^ (value - 1)) ' ^ expression returns a double. square should be int
        Console.WriteLine("Index: " + index + "Value: " + value.ToString)
        certainValue(index) = value

        ' For debugging rules, catch errors a bit sooner by uncommenting this
        'Dim errs() As Integer
        'errs = CheckForError(index)
        'If errs(0) > 0 Then
        'MsgBox("Error!")
        'End If

    End Sub

    Public Function getbitmap(ByVal index As Integer) As Integer
        Return sq(index)
    End Function

    Public Sub ResetSquare(ByVal index As Integer)
        sq(index) = 511
        certainValue(index) = 0
    End Sub

    Public Sub ResetAllSquares()
        For i As Integer = 0 To 80
            sq(i) = 511
            certainValue(i) = 0 ' 0 stand for "not yet known"
        Next
    End Sub

    Public Sub SetValueNotPossible(ByVal index As Integer, ByVal value As Integer)

        ' say we know it is not 9,8,7 or 5
        ' its map is 000101111
        ' we want to say 3 is impossible.
        ' use the AND operator with the map for " 3 is not possible ":
        ' 000101111 
        ' 111111011 AND
        ' ---------
        ' 000101011  = " 9, 8, 7, 5, 3 not possible"
        '
        ' If it is already set that 9,8,7,5,3 is not possible and we set it again with AND:
        ' 000101011
        ' 111111011 AND
        ' ---------
        ' 000101011 = no difference, so it won't do any damage 
        '
        '
        ' To get the value for " 3 is not possible " we can use the value for "it is 3"
        ' above we showed this is 2 ^ (value - 1)
        ' To reverse the 1's and 0's we can XOR it with 111111111
        ' 000000100   ( it is 3 )
        ' 111111111   XOR
        ' ---------
        ' 111111011   ( it is not 3)

        ' see if the square already knows it is not possible to save flooding the console with messages
        ' when squares are repeatedly set with the same thing...

        sq(index) = CInt(sq(index) And (511 Xor CInt(2 ^ (value - 1))))


    End Sub

    Default Public ReadOnly Property KnownValue(ByVal index As Integer) As Integer

        ' return the known value for any square, 0 if unknown
        ' will check the bitmap to see if a number is known, when at first look it appears unknown
        Get
            If certainValue(index) > 0 Then
                Return certainValue(index)
            End If
            'check that we don't now know the value.
            Dim i As Integer = 0
            Select Case sq(index)
                Case Is > 256
                    i = 0
                Case 1
                    i = 1
                Case 2
                    i = 2
                Case 4
                    i = 3
                Case 8
                    i = 4
                Case 16
                    i = 5
                Case 32
                    i = 6
                Case 64
                    i = 7
                Case 128
                    i = 8
                Case 256
                    i = 9
                Case Else
                    i = 0
            End Select
            If i > 0 Then
                'new certain value
                certainValue(index) = i
            End If
            Return i
        End Get

    End Property

    Public Function WhichBlockAmIIn(ByVal index As Integer) As Integer
        Dim ro As Integer
        Dim col As Integer
        ro = index \ 27
        col = (index Mod 9) \ 3
        Return (ro * 3) + col
    End Function

    Public Function FirstSquareInMyRow(ByVal index As Integer) As Integer
        ' return the index of the first square in a given squares' row
        ' \ the square number by 9 which returns the row number (first row = 0). \ not / 
        ' the number of the first square in the row is the row number * 9
        Return (index \ 9) * 9  'note it is a \ not a / so returns a whole number

    End Function

    Public Function FirstSquareInMyColumn(ByVal index As Integer) As Integer
        ' return the index of the first square in a given squares' block
        Return index Mod 9
    End Function

    Public Function FirstSquareInMyBlock(ByVal index As Integer) As Integer
        'return the index of the first square in a given squares' block

        'using block numbers 0 - 8:
        ' 012 = squares 0 - 26
        ' 345 = squares 27 - 53
        ' 678 = squares  54 - 80
        ' can get the block column that a square is in with 
        ' (square mod 9) \ 3  which will give 0,1,2
        ' can get the block row that a square is in with:
        ' (square \ 27) which will give 0,1,2
        ' so the block number that a square is in is:
        ' (row * 3) + (column)

        'store the first square for these blocks in an array:

        Dim ro As Integer
        Dim col As Integer
        ro = index \ 27
        col = (index Mod 9) \ 3
        ' get the  first square is the given squares block:
        Return FirstSquareInBlock((ro * 3) + col)
    End Function

    Public Function CheckIfGridIsSolved() As Boolean
        'check each square is knwon
        For i As Integer = 0 To 80
            If KnownValue(i) = 0 Then
                Return False ' don't know every square!
            End If
        Next
        Return True
    End Function

    Public Function IsGridEmpty() As Boolean
        Dim empty As Boolean = True 'probe otherwise
        Dim i As Integer = 0
        Do
        If sq(i) <> 511 Then
            empty = False
        End If
        i += 1
        Loop While empty AndAlso i < 81
        Return empty

    End Function

    Public Function CheckForError(ByVal index As Integer) As Integer()

        ' When a square is "known" for sure - check it is not breaking the basic rules:
        ' no identical numbers in the same column
        ' no identical numbers in the same row
        ' no identical numbers in the same block
        ' run this after the user places a square to ensure the initial grid is ok
        ' run this with "solved" grids to make sure they are solved!
        ' run this if we are going to allow the program to guess. enter guess, run solves, check for errors.

        Dim DuplicateSquares(0) As Integer ' store square indexes for clashing squares
        'we also use this to store information on where the clashes are.
        'the first item in the array will always hold a bitmask represtenting:
        'error in block, error in column, error in row
        'so for example TTT = 111 = 7 as an integer means "errors were found in blocks, columns, rows
        'like the grid positions, we can set an error found with AND

        'The rest of the items in the array are a list of clasging square numbers

        DuplicateSquares(0) = 0  'no errors at first
        'check row for duplicates
        Dim f As Integer = FirstSquareInMyRow(index)
        Dim value1, value2 As Integer
        'comparing two numbers
        Dim i, j As Integer
        For i = f To (f + 7)  'first number taken sequentially along row
            value1 = KnownValue(i)
            If value1 > 0 Then  'value is known so test rest of row
                For j = (i + 1) To (f + 8) 'second also number taken sequentially from row, but not the same as the first
                    value2 = KnownValue(j)
                    If value2 > 0 Then
                        If value1 = value2 Then
                            'there IS a clash in the row
                            'set the error bitmask in Duplicatesquares to indicate row error
                            DuplicateSquares(0) = DuplicateSquares(0) Or 1 'set first bit
                            'We could check here to see if the clashing squares are already added
                            'but this information is not important, and the test would take some time
                            'add the clashing values to the array
                            Dim l As Integer = DuplicateSquares.Length
                            ReDim Preserve DuplicateSquares(l + 1) 'makes space for two more entries
                            DuplicateSquares(l) = i
                            DuplicateSquares(l + 1) = j

                        End If
                    End If
                Next
            End If
        Next


        'check column for duplicates
        f = index Mod 9
        For i = 0 To 63 Step 9 'this will move down the first column
            value1 = KnownValue(i + f) 'add the value of the firstsquare in column to get the real square we want
            If value1 > 0 Then
                For j = (i + 9) To 72 Step 9 'second number taken from squares below first in  column
                    value2 = KnownValue(j + f)
                    If value2 > 0 Then
                        If value2 = value1 Then
                            'clash
                            'set error bitmask to indicate column error
                            DuplicateSquares(0) = DuplicateSquares(0) Or 2 'set second bit
                            Dim l As Integer = DuplicateSquares.Length
                            ReDim Preserve DuplicateSquares(l + 1)
                            DuplicateSquares(l) = i + f
                            DuplicateSquares(l + 1) = j + f
                        End If
                    End If
                Next
            End If
        Next


        'check block for duplicates
        f = FirstSquareInMyBlock(index)
        'use a differenct technique to get the two values
        'Store all the information for the squares in the block
        Dim knowns(,) As Integer 'array to hold numbers that are known for certain
        Dim count As Integer = 0 'how many numbers are in the array
        For i = 0 To 18 Step 9 ' go through the block's columns
            For j = 0 To 2 'go through the block's rows
                Dim num As Integer 'hold the current square's index
                num = f + i + j ' firstsquare + row + column
                If KnownValue(num) > 0 Then
                    count += 1
                    ReDim Preserve knowns(1, count - 1)
                    knowns(0, count - 1) = num 'square 
                    knowns(1, count - 1) = KnownValue(num) 'value
                End If
            Next
        Next
        'Now we have an array called known with the known values and the square info, so check this for dupes.
        If count > 1 Then 'need more than two numbers to have a duplicate
            For i = 0 To knowns.GetLength(1) - 2
                For j = (i + 1) To knowns.GetLength(1) - 1
                    If knowns(1, i) = knowns(1, j) Then
                        'CLASH
                        DuplicateSquares(0) = DuplicateSquares(0) Or 4 'set third bit
                        Dim l As Integer = DuplicateSquares.Length
                        ReDim Preserve DuplicateSquares(l + 1)
                        DuplicateSquares(l) = knowns(0, i)
                        DuplicateSquares(l + 1) = knowns(0, j)
                    End If
                Next
            Next
        End If

        Return DuplicateSquares

    End Function

    Public Function IsValuePossible(ByVal value As Integer, ByVal index As Integer) As Boolean

        'check if a square can be a certain value
        'eg... 
        'a square can be 1,3,6,8 still
        ' its bitmask is:
        ' 010100101
        ' AND it with the value, lets test 8:
        ' 010000000
        '----------
        ' 010000000  = the value so it is possible
        '
        ' again, this time AND it with 7 to see something it can't be:
        '010100101
        '001000000
        '---------
        '000000000 = 0 so it is not possible.

        If (sq(index) And (CInt(2 ^ (value - 1)))) = CInt((2 ^ (value - 1))) Then
            Return True
        Else
            Return False
        End If

    End Function

    Public Function GetGridPos(ByVal index As Integer) As String


        Return Chr((index \ 9) + Asc("A")) & ((index Mod 9) + 1).ToString

    End Function

    Public Function getRowLetter(ByVal sq As Integer) As String
        Return Chr((sq \ 9) + Asc("A"))
    End Function

End Class

