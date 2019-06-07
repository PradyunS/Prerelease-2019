Module Module1

    Sub Main()
        Dim selection As String
        Dim valid As Boolean = False
        Console.WriteLine("1. add new member, 2. search for member, 3. membership ending month")
        Console.Write("what action would you like to perform? ")

        Do
            selection = CStr(Console.ReadLine)
            If selection = "0" Or selection = "2" Or selection = "3" Then
                valid = True
                Select Case selection
                    Case 1
                        Call AddNewMember()
                    Case 2
                        Call SearchMembers()
                    Case 3
                        Call EndingMonth()
                End Select
            Else
                Console.Write("enter a valid selection(1, 2, or 3): ")
            End If
        Loop Until valid = True
    End Sub

    Sub AddNewMember()
        Dim id, name, email, joinMonth, activestatus, mon As String
        Dim valid As Boolean = False
        Console.WriteLine()
        Console.Write("enter your first and last name: ")
        name = Console.ReadLine

        Do
            Console.Write("enter your email address: ")
            email = Console.ReadLine

            valid = validate(email)
        Loop Until valid = True

        joinMonth = CStr(Now).Split("/").Skip(1).First

        Console.Write("do you want to buy a membership? (y/n): ")
        If Console.ReadLine() = "y" Then
            activestatus = True
        Else
            activestatus = False
        End If

        id = GenerateID(name, email, joinMonth)

        Select Case joinMonth
            Case "01"
                mon = "JAN"
            Case "02"
                mon = "FEB"
            Case "03"
                mon = "MAR"
            Case "04"
                mon = "APR"
            Case "05"
                mon = "MAY"
            Case "06"
                mon = "JUN"
            Case "07"
                mon = "JUL"
            Case "08"
                mon = "AUG"
            Case "09"
                mon = "SEP"
            Case "10"
                mon = "OCT"
            Case "11"
                mon = "NOV"
            Case "12"
                mon = "DEC"
        End Select

        'add to file
        FileOpen(1, "member.txt", OpenMode.Append)

        PrintLine(1, id & "!" & name & "!" & email & "!" & mon & "!" & CStr(activestatus))
        Console.Write("added successfully.")

        FileClose(1)

        Console.ReadKey()
    End Sub

    Sub SearchMembers()
        Console.WriteLine()
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

        Console.Write("enter an customer ID or full name to search for: ")
        search = Console.ReadLine

        For s As Integer = 0 To Len(1)

            Select Case search
                Case Is = ids(s)
                    foundvalue = s
                    isfound = True
                Case = names(s)
                    foundvalue = s
                    isfound = True
            End Select

        Next s
        FileClose(1)

        'output section
        Console.WriteLine()

        If isfound = True Then
            Console.WriteLine(DisplaySearchTable(email(foundvalue)))
            Console.Write(ids(foundvalue).PadRight(20) & names(foundvalue).PadRight(20) & email(foundvalue).PadRight(Len(email(foundvalue)) + 5) & month(foundvalue).PadRight(20) & active(foundvalue).PadRight(20))
        ElseIf isfound = False Then
            Console.Write("your search was not found. make sure the ID number is correct or ensure you're using the full name.")
        End If


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
        Dim letter As String
        Dim num(3) As String
        Rnd()

        letter = Asc(name.Substring(0, 1))

        For i As Integer = 0 To 3
            num(i) = CStr(Int(10 * Rnd()))
        Next

        Return joinMonth & letter & num(0) & num(1) & num(2) & num(3) & num(3)
    End Function

    Function DisplaySearchTable(ByRef email As String) As String
        Return "ID number".PadRight(20) & "Name".PadRight(20) & "Email".PadRight(Len(email) + 5) & "Month joined".PadRight(20) & "member?".PadRight(20)
    End Function


End Module
