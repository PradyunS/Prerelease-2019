Module Module1


    Sub Main()
        'Dim id, name, email, joinMonth, activestatus As String

        Console.WriteLine("1. add new member, 2. search for member, 3. membership ending month")
        Console.Write("what action would you like to perform? ")
        Console.WriteLine()

        Select Case Console.ReadLine()
            Case 1
                Call AddNewMember()
            Case 2
                Call SearchMembers()
            Case 3
                Call EndingMonth()
        End Select

    End Sub

    Sub AddNewMember()
        Dim id, name, email, joinMonth, activestatus As String
        Dim valid As Boolean = False

        Console.Write("enter your first and last name: ")
        name = Console.ReadLine

        Do
            Console.Write("enter your email address: ")
            email = Console.ReadLine

            valid = validate(email)
        Loop Until valid = True

        joinMonth = "JUN"

        Console.Write("do you want to buy a membership? (y/n): ")

        If Console.ReadLine() = "y" Then
            activestatus = True
        Else
            activestatus = False
        End If

        id = GenerateID(name, email, joinMonth)

        'add to file
        FileOpen(1, "member.txt", OpenMode.Append)

        PrintLine(1, id & "!" & name & "!" & email & "!" & joinMonth & "!" & CStr(activestatus))
        Console.Write("added successfully.")

        FileClose(1)

    End Sub

    Sub SearchMembers()
        FileOpen(1, "member.txt", OpenMode.Input)
        Dim names(Len(1)), email(Len(1)), values(Len(1)), ids(Len(1)), search, month(Len(1)), active(Len(1)) As String
        Dim isfound As Boolean = False
        Dim i, foundvalue As Integer

        While Not EOF(1)
            values(i) = LineInput(1)
            ids(i) = values(i).Split("!").First
            names(i) = values(i).Split("!").Skip(1).First
            email(i) = values(i).Split("!").Skip(2).First
            month(i) = values(i).Split("!").Skip(3).First
            active(i) = values(i).Split("!").Skip(4).First
            i += 1
        End While

        Console.Write("enter an customer ID or name to search for: ")
        search = Console.ReadLine



        For s As Integer = 0 To Len(1)
            Select Case search
                Case = ids(s)
                    foundvalue = s
                    isfound = True
                Case = names(s)
                    foundvalue = s
                    isfound = True
            End Select
        Next

        'output section
        Console.WriteLine()

        If isfound = True Then
            Console.WriteLine(DisplaySearchTable(email(foundvalue)))
            Console.Write(ids(foundvalue).PadRight(20) & names(foundvalue).PadRight(20) & email(foundvalue).PadRight(Len(email(foundvalue)) + 5) & month(foundvalue).PadRight(20) & active(foundvalue).PadRight(20))
        ElseIf isfound = False Then
            Console.Write("your search was not found")
        End If



        '''''HERE THERE BE MONSTERS''''''
        ' Dim test(9, 99) As String
        '
        ' For a As Integer = 0 To 9
        '     For b As Integer = 0 To 99
        '         test(a) = values(0).Split("!", 1)
        '     Next
        ' Next

        FileClose(1)
        Console.ReadKey()
    End Sub

    Sub EndingMonth()

    End Sub

    Function validate(ByRef email As String) As Boolean
        If email.IndexOf("@") >= 0 And email.IndexOf("@") <= email.Length Then
            Return True
        Else
            Return False
        End If

    End Function

    Function GenerateID(ByRef name As String, ByRef email As String, ByRef joinMonth As String) As String
        Dim mon, letter As String
        Dim num(3) As String
        Rnd()

        letter = Asc(name.Substring(0, 1))
        Select Case joinMonth
            Case "JAN"
                mon = "01"
            Case "FEB"
                mon = "02"
            Case "MAR"
                mon = "03"
            Case "APR"
                mon = "04"
            Case "MAY"
                mon = "05"
            Case "JUN"
                mon = "06"
            Case "JUL"
                mon = "07"
            Case "AUG"
                mon = "08"
            Case "SEP"
                mon = "09"
            Case "OCT"
                mon = "10"
            Case "NOV"
                mon = "11"
            Case "DEC"
                mon = "12"
        End Select

        For i As Integer = 0 To 3
            num(i) = CStr(Int(10 * Rnd()))
        Next

        Return mon & letter & num(0) & num(1) & num(2) & num(3)
    End Function

    Function DisplaySearchTable(ByRef email As String) As String
        Return "ID number".PadRight(20) & "Name".PadRight(20) & "Email".PadRight(Len(email) + 5) & "Month joined".PadRight(20) & "member?".PadRight(20)
    End Function


End Module
