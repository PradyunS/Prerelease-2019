Module Module1
    Dim recordCount As Integer = LoadValues()


    Sub Main()
        Dim selection As String
        Dim valid As Boolean = False

        'display title sequence
        Console.WriteLine("#########################################################")
        Console.WriteLine("##########  Dwight Schrute's Gym For Muscules  ##########")
        Console.WriteLine("#################  (and other stuff)  ###################")
        Console.WriteLine("#########################################################")

        Console.WriteLine("
        1. add new member
        2. search for member
        3. membership ending month
        4. password generator
        5. tip calculator
        6. random number generator
        7. battleships
        8. the entirety of 2018's prerelease
        9. rock paper scissors
        10. Ding!
         ")
        Console.Write("what action would you like to perform? ")
        Do
            selection = CStr(Console.ReadLine)
            If selection = "1" Or selection = "2" Or selection = "3" Or selection = "4" Or selection = "5" Or selection = "6" Or selection = "7" Or selection = "8" Or selection = "9" Or selection = "10" Then
                valid = True
                Console.Clear()
                Select Case selection
                    Case 1
                        Call AddNewMember()
                    Case 2
                        Call SearchMembers()
                    Case 3
                        Call EndingMonth()
                    Case 4
                        Call passwordGenerator()
                    Case 5
                        Call tipCalculate()
                    Case 6
                        Call randomNum()
                    Case 7
                        Call battleship()
                    Case 8
                        Call cabinBook()
                    Case 9
                        Call rps()
                    Case 10
                        Call ding()
                End Select
            Else
                Console.Write("enter a valid selection (1-9) ")
            End If
        Loop Until valid = True
    End Sub

    Sub AddNewMember()
        Dim id, name, email, joinMonth, activestatus, mon As String
        Dim valid As Boolean = False
        joinMonth = CStr(Now).Split("/").Skip(1).First

        Console.WriteLine()
        Console.Write("enter your first and last name: ")
        name = Console.ReadLine & " "

        Do
            Console.Write("enter your email address: ")
            email = Console.ReadLine
            valid = validate(email)
        Loop Until valid = True

        Console.Write("do you want to buy a membership? (y/n): ")
        If Console.ReadLine() = "y" Then
            activestatus = True
        ElseIf Console.ReadLine() = "n" Then
            activestatus = False
        Else
            activestatus = True
            Console.Write("oh you wanna try crash this? you paying for membership.")
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

        'update recordcount
        FileOpen(1, "savedata.txt", OpenMode.Output)
        Print(1, recordCount + 1)
        FileClose(1)

        Call goHome()
    End Sub

    Sub SearchMembers()
        Console.WriteLine()
        Dim names(recordCount), email(recordCount), values(recordCount), ids(recordCount), search, month(recordCount), active(recordCount) As String
        Dim isfound As Boolean = False
        Dim i As Integer = 0
        Dim foundvalue As Integer

        FileOpen(1, "member.txt", OpenMode.Input)
        While Not EOF(1)
            values(i) = LineInput(1)
            i += 1
        End While
        FileClose(1)

        For a As Integer = 0 To recordCount
            ids(a) = values(a).Split("!").First
            names(a) = values(a).Split("!").Skip(1).First
            email(a) = values(a).Split("!").Skip(2).First
            month(a) = values(a).Split("!").Skip(3).First
            active(a) = values(a).Split("!").Skip(4).First
        Next

        Console.Write("enter an customer ID, or name to search for: ")
        search = Console.ReadLine.ToLower

        For s As Integer = 0 To recordCount
            Select Case search
                Case Is = ids(s)
                    foundvalue = s
                    isfound = True
                Case = names(s).ToLower
                    foundvalue = s
                    isfound = True
                Case = names(s).Split(" ").First.ToLower
                    foundvalue = s
                    isfound = True
                    'Case = month(s).ToLower
                    '    foundvalue = s
                    '    isfound = True
            End Select
        Next s

        'output section
        Console.WriteLine()

        If isfound = True Then
            Console.WriteLine(DisplaySearchTable())
            Console.Write(ids(foundvalue).PadRight(20) & names(foundvalue).PadRight(20) & email(foundvalue).PadRight(40) & month(foundvalue).PadRight(20) & active(foundvalue).PadRight(20))
        ElseIf isfound = False Then
            Console.Write("your search was not found. make sure the ID number or name is correct")
        End If

        Call goHome()

    End Sub

    Sub EndingMonth()
        Dim values(recordCount), names(recordCount), email(recordCount), ids(recordCount), active(recordCount), month(recordCount), found(99) As String
        Dim i As Integer = 0
        Dim input As Integer
        Dim flag As Boolean = False


        FileOpen(1, "member.txt", OpenMode.Input)
        While Not EOF(1)
            values(i) = LineInput(1)
            i += 1
        End While
        FileClose(1)

        For a As Integer = 0 To recordCount
            ids(a) = values(a).Split("!").First
            names(a) = values(a).Split("!").Skip(1).First
            email(a) = values(a).Split("!").Skip(2).First
            month(a) = values(a).Split("!").Skip(3).First
            active(a) = values(a).Split("!").Skip(4).First
        Next

        Console.Write("enter the month you would like to view as a number (01 to 12): ")
        input = Console.ReadLine

        Console.WriteLine(DisplaySearchTable)
        For j As Integer = 0 To recordCount
            If input = CInt(values(j).Substring(1, 1)) Then
                Console.WriteLine(ids(j).PadRight(20) & names(j).PadRight(20) & email(j).PadRight(40) & month(j).PadRight(20) & active(j).PadRight(20))
                flag = True
            ElseIf j = recordCount And flag = False Then
                Console.WriteLine("no members found")
                'Exit For
            End If
        Next

        If input < CStr(Now).Split("/").Skip(1).First Then
            Console.Write("these members' membership has expired. do you want to save their information to a new file? (y/n): ")
            If Console.ReadLine = "y" Then
                FileOpen(1, "expiredmembers.txt", OpenMode.Append)
                For m As Integer = 0 To recordCount
                    If values(m).Substring(0, 2) < CStr(Now).Split("/").Skip(1).First Then
                        PrintLine(1, names(m).PadRight(20) & email(m).PadRight(40))
                    End If
                Next
                Console.WriteLine("success")
                FileClose(1)
            End If
        End If

        Call goHome()

    End Sub

    Sub passwordGenerator()
        Dim length As Integer
        Dim input, password As String
        Randomize()

        Console.Write("how long should your password be in characters: ")
        input = Console.ReadLine
        Dim chars(input) As Char


        For i As Integer = 0 To input
            chars(i) = Chr(Int(125 * Rnd() + 31))
            password += chars(i)
        Next

        Console.WriteLine("your password is: " & password)

        Call goHome()
    End Sub

    Sub tipCalculate()
        Dim price, tip As Double
        Dim tipAmount As Integer = 15

        Console.Write("how much is your meal?: ")
        price = Console.ReadLine
        Console.Write("what is the percentage you would like to tip: ")
        tipAmount = Console.ReadLine

        tip = price * (tipAmount / 100)

        Console.WriteLine("your tip amount is: " & tip & ", bringing your total to " & price + tip)

        Call goHome()
    End Sub

    Sub randomNum()
        Dim length, num As Integer
        Randomize()

        Console.Write("how long should the number be? ")
        length = Console.ReadLine

        num = CInt(Int(length * Rnd()))

        Console.WriteLine("your number is (drumroll please): " & num)

        Call goHome()
    End Sub

    Sub battleship()
        Dim board(7, 7) As String
        Dim ix, iy As String
        Dim aihit(1) As String

        For r As Integer = 0 To 7
            For c As Integer = 0 To 7
                board(r, c) = "~"
            Next c
        Next r

        'setting up ships
        For i As Integer = 1 To 3
            board(2, i) = "S"
        Next
        For i As Integer = 1 To 5
            board(i, 6) = "S"
        Next

        Console.Write("  1 2 3 4 5 6 7 8") 'display x coords
        For r As Integer = 0 To 7  'display board
            Console.WriteLine()
            Console.Write(r + 1 & " ") 'display y coords
            For c As Integer = 0 To 7
                Console.Write(board(r, c).PadRight(2))
            Next c
        Next r

        Console.WriteLine()

        For x As Integer = 1 To 10
            Console.Write("enter x coordinate to hit: ")
            ix = CStr(Console.ReadLine) - 1 '-1 bc array 

            Console.Write("enter y coordinate to hit: ")
            iy = CStr(Console.ReadLine) - 1


            'If 0 = 0 Then
            'Console.Write("you have hit a ship! ")
            'End If
            'i guess they never miss huh


            If board(CInt(iy), CInt(ix)) = "S" Then
                Console.Write("you have hit a ship! ")
                board(iy, ix) = "*"
            ElseIf board(iy, ix) = "~" Then
                Console.Write("you have missed")
                board(iy, ix) = "#"
            End If
            Console.WriteLine()

            Console.Write("  1 2 3 4 5 6 7 8")
            For r As Integer = 0 To 7  'display board
                Console.WriteLine()
                Console.Write(r + 1 & " ")
                For c As Integer = 0 To 7
                    Console.Write(board(r, c).PadRight(2))
                Next c
            Next r

            Threading.Thread.Sleep(1000)
            Console.WriteLine()

            For i As Integer = 0 To 1
                aihit(i) = CStr(Int(8 * Rnd() + 1))
            Next i


            If board(CInt(aihit(0)), CInt(aihit(1))) = "S" Then
                Console.Write("Computer has hit a ship " & aihit(0) & ", " & aihit(1))
                board(aihit(0), aihit(1)) = "*"
            ElseIf board(aihit(0), aihit(1)) = "~" Then
                Console.Write("computer has missed ")
                board(aihit(0), aihit(1)) = "#"
            End If
            Console.WriteLine()

            Console.Write("  1 2 3 4 5 6 7 8")
            For r As Integer = 0 To 7  'display board
                Console.WriteLine()
                Console.Write(r + 1 & " ")
                For c As Integer = 0 To 7
                    Console.Write(board(r, c).PadRight(2))
                Next c
            Next r
            Console.WriteLine()
        Next x

    End Sub

    Sub cabinBook()
        Dim cabinName() As String = {"Hetty", "Poppy", "Blue Skies", "Bay View", "Happy Days", "Summer Joy", "Walkers' Rest", "Bertie", "Green Forest Lodge", "Coppice Lodge"}
        Dim capacity() As Integer = {4, 4, 4, 6, 6, 6, 8, 8, 10, 10}
        Dim peak() As Decimal = {400, 400, 500, 650, 695, 800, 950, 1050, 1200, 1500}
        Dim offPeak() As Decimal = {250, 250, 350, 500, 550, 600, 750, 850, 950, 1150}
        Dim booked(16, 9) As Boolean
        Dim bkcode(16, 9) As Integer
        Dim bookedweeks As Integer = 0

        Randomize()

        Dim cabinChoice, weekNumber As Integer
        Dim cost As Decimal

        ' bookings 2D array - rows: 17 weeks, columns: 10 log cabins
        ' initialise bookings array - no bookings made at program start
        For weeks As Integer = 0 To UBound(booked)
            For cabins As Integer = 0 To UBound(cabinName)
                booked(weeks, cabins) = False
            Next cabins
        Next weeks

        Console.WriteLine()

        Console.WriteLine("#  HOLIDAY PARK BOOKING SYSTEM  #")

        Console.WriteLine()

        For i As Integer = 0 To UBound(cabinName)
            Console.WriteLine(CStr(i + 1 & ". " & cabinName(i)))
        Next i

        Console.WriteLine()

        Do
            Console.Write("Which cabin do you want to look at? Enter a number: ")
            cabinChoice = CInt(Trim(Console.ReadLine))
            If (cabinChoice >= 1) And (cabinChoice <= 10) Then
                Exit Do
            Else
                Console.WriteLine("Error - invalid cabin choice. Try again.")
            End If
        Loop



        Do
            Console.Write("Enter a week to display bookings - weeks 23 to 39: ")
            weekNumber = CInt(Trim(Console.ReadLine))
            If (weekNumber >= 23) And (weekNumber <= 39) Then
                Exit Do
            Else
                Console.WriteLine("Error - you may only choose from weeks 23 to 39, inclusive.")
            End If
        Loop

        Console.WriteLine()

        For i As Integer = 0 To UBound(cabinName)
            If i = cabinChoice - 1 Then
                Select Case weekNumber
                    Case 23 To 26, 36 To 39 : cost = offPeak(i)
                        Console.WriteLine("WEEK NUMBER".PadRight(20) & "CABIN NAME".PadRight(20) & "CAPACITY".PadRight(20) & "COST".PadRight(20) & "BOOKING STATUS")
                        Console.Write(("Week " & CStr(weekNumber)).PadRight(20) & cabinName(i).PadRight(20) & CStr(capacity(i)).PadRight(20) & FormatCurrency(offPeak(i)).PadRight(20))
                        weekNumber = weekNumber - 23
                        If booked(weekNumber, cabinChoice) = False Then
                            Console.WriteLine("Not booked")
                        Else
                            Console.WriteLine("Booked")
                        End If
                    Case 27 To 35 : cost = peak(i)
                        Console.WriteLine("WEEK NUMBER".PadRight(20) & "CABIN NAME".PadRight(20) & "CAPACITY".PadRight(20) & "COST".PadRight(20) & "BOOKING STATUS")
                        Console.Write(("Week " & CStr(weekNumber)).PadRight(20) & cabinName(i).PadRight(20) & CStr(capacity(i)).PadRight(20) & FormatCurrency(peak(i)).PadRight(20))
                        weekNumber = weekNumber - 23
                        If booked(weekNumber, cabinChoice) = False Then
                            Console.WriteLine("Not booked")
                        Else
                            Console.WriteLine("Booked")
                        End If
                End Select
            End If
        Next i

        Console.WriteLine(weekNumber)
        Do
            Console.Write("how long will you be staying: ")
            bookedweeks = CInt(Console.ReadLine)
            If bookedweeks + weekNumber >= 17 Then
                Console.WriteLine("sorry, we wont be open for that long. choose a smaller number.")
            ElseIf bookedweeks + weekNumber < 17 Then
                For i As Integer = 0 To bookedweeks
                    bkcode(weekNumber + i, cabinChoice) = (CInt(Int(170 * Rnd() + 1)))
                Next i
                Exit Do
            End If
        Loop

        If bookedweeks >= 3 Then
            Select Case weekNumber
                Case 23 To 26, 36 To 39
                    cost = offPeak(cabinChoice)
                Case 27 To 35
                    cost = peak(cabinChoice)
            End Select
            cost = cost * 0.9
            Console.Write("your final price, with a 10% discount applied, is: " & FormatCurrency(cost) & " rather than " & FormatCurrency(cost / 0.9))
        ElseIf bookedweeks < 3 Then
            Console.Write("your final price is: " & FormatCurrency(cost))
        End If

        Call goHome()

    End Sub

    Sub rps()
        Dim p1input, p2input, output As String
        Randomize()

        Console.Write("your move. rock (r) paper (p) scissors(s)")
        p1input = Console.ReadLine

        p2input = CStr(Int(3 * Rnd() + 1))

        Select Case p2input  '1 = r, 2 = p, 3 = s
            Case "1"
                If p1input = "r" Then
                    Console.Write("tie")
                ElseIf p1input = "p" Then
                    Console.Write("lose")
                ElseIf p1input = "s" Then
                    Console.Write("win")
                End If
            Case "2"
                If p1input = "r" Then
                    Console.Write("lose")
                ElseIf p1input = "p" Then
                    Console.Write("tie")
                ElseIf p1input = "s" Then
                    Console.Write("win")
                End If
            Case "3"
                If p1input = "r" Then
                    Console.Write("win")
                ElseIf p1input = "p" Then
                    Console.Write("lose")
                ElseIf p1input = "s" Then
                    Console.Write("tie")
                End If
        End Select

        Call goHome()
    End Sub

    Sub ding()
        My.Computer.Audio.Play("ding.wav", AudioPlayMode.WaitToComplete)

        Call goHome()
    End Sub


    Sub goHome()
        Console.WriteLine()
        Console.Write("enter h to go back home or enter key to exit: ")
        If Console.ReadLine = "h" Then
            Console.Clear()
            Call Main()
        End If
    End Sub

    Function validate(ByRef email As String) As Boolean
        If email.IndexOf("@") >= 0 And email.IndexOf("@") <= email.Length & email.IndexOf(".") >= 0 & email.IndexOf(".") <= email.Length Then
            Return True
        Else
            Return False
        End If
    End Function

    Function GenerateID(ByRef name As String, ByRef email As String, ByRef joinMonth As String) As String
        Dim letter, num(3) As String
        Randomize()

        letter = Asc(name.Substring(0, 1).ToUpper)

        For i As Integer = 0 To 3
            num(i) = CStr(Int(10 * Rnd()))
        Next

        Return joinMonth & letter & num(0) & num(1) & num(2) & num(3)
    End Function

    Function DisplaySearchTable() As String
        Return "ID number".PadRight(20) & "Name".PadRight(20) & "Email".PadRight(40) & "Month joined".PadRight(20) & "member?".PadRight(20)
    End Function

    Function LoadValues()
        Dim values As String
        FileOpen(1, "savedata.txt", OpenMode.Input)
        values = LineInput(1)
        FileClose(1)
        Return values
    End Function

End Module
