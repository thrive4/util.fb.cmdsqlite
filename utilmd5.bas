' MD5 encrypt from the Wikipedia page "MD5"
' compile with: fbc -s console
' from https://rosettacode.org/wiki/MD5/Implementation#FreeBASIC
' note md5 is not reversible, at least it shouldn't be...
' added basic file i/o thrive4 2022

' macro for a rotate left
#Macro ROtate_Left (x, n) ' rotate left
  (x) = (x) Shl (n) + (x) Shr (32 - (n))
#EndMacro

Function MD5(test_str As String) As String

    Dim As String message = test_str   ' strings are passed as ByRef's

    Dim As UByte sx, s(0 To ...) = { 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, _
    17, 22,  7, 12, 17, 22,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20, _
    5,  9, 14, 20,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, _
    16, 23,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 }

    Dim As UInteger<32> K(0 To ...) = { &Hd76aa478, &He8c7b756, &H242070db, _
    &Hc1bdceee, &Hf57c0faf, &H4787c62a, &Ha8304613, &Hfd469501, &H698098d8, _
    &H8b44f7af, &Hffff5bb1, &H895cd7be, &H6b901122, &Hfd987193, &Ha679438e, _
    &H49b40821, &Hf61e2562, &Hc040b340, &H265e5a51, &He9b6c7aa, &Hd62f105d, _
    &H02441453, &Hd8a1e681, &He7d3fbc8, &H21e1cde6, &Hc33707d6, &Hf4d50d87, _
    &H455a14ed, &Ha9e3e905, &Hfcefa3f8, &H676f02d9, &H8d2a4c8a, &Hfffa3942, _
    &H8771f681, &H6d9d6122, &Hfde5380c, &Ha4beea44, &H4bdecfa9, &Hf6bb4b60, _
    &Hbebfbc70, &H289b7ec6, &Heaa127fa, &Hd4ef3085, &H04881d05, &Hd9d4d039, _
    &He6db99e5, &H1fa27cf8, &Hc4ac5665, &Hf4292244, &H432aff97, &Hab9423a7, _
    &Hfc93a039, &H655b59c3, &H8f0ccc92, &Hffeff47d, &H85845dd1, &H6fa87e4f, _
    &Hfe2ce6e0, &Ha3014314, &H4e0811a1, &Hf7537e82, &Hbd3af235, &H2ad7d2bb, _
                                                              &Heb86d391 }

    ' Initialize variables
    Dim As UInteger<32> A, a0 = &H67452301
    Dim As UInteger<32> B, b0 = &Hefcdab89
    Dim As UInteger<32> C, c0 = &H98badcfe
    Dim As UInteger<32> D, d0 = &H10325476
    Dim As UInteger<32> dtemp, F, g, temp

    Dim As Long i, j

    Dim As ULongInt l = Len(message)
    ' set the first bit after the message to 1
    message = message + Chr(1 Shl 7)
    ' add one char to the length
    Dim As ULong padding = 64 - ((l +1) Mod (512 \ 8)) ' 512 \ 8 = 64 char.

    ' check if we have enough room for inserting the length
    If padding < 8 Then padding = padding + 64

    message = message + String(padding, Chr(0))   ' adjust length
    Dim As ULong l1 = Len(message)                ' new length

    l = l * 8    ' orignal length in bits
    ' create ubyte ptr to point to l ( = length in bits)
    Dim As UByte Ptr ub_ptr = Cast(UByte Ptr, @l)

    For i = 0 To 7  'copy length of message to the last 8 bytes
    message[l1 -8 + i] = ub_ptr[i]
    Next

    For j = 0 To (l1 -1) \ 64 ' split into block of 64 bytes

    A = a0 : B = b0 : C = c0 : D = d0

    ' break chunk into 16 32bit uinteger
    Dim As UInteger<32> Ptr M = Cast(UInteger<32> Ptr, @message[j * 64])

    For i = 0 To 63
      Select Case As Const i
        Case 0 To 15
          F = (B And C) Or ((Not B) And D)
          g = i
        Case 16 To 31
          F = (B And D) Or (C And (Not D))
          g = (i * 5 +1) Mod 16
        Case 32 To 47
          F = (B Xor C Xor D)
          g = (i * 3 +5) Mod 16
        Case 48 To 63
          F = C Xor (B Or (Not D))
          g = (i * 7) Mod 16
      End Select
      dtemp = D
      D = C
      C = B
      temp = A + F + K(i)+ M[g] : ROtate_left(temp, s(i))
      B = B + temp
      A = dtemp
    Next

    a0 += A : b0 += B : c0 += C : d0 += D

    Next

    Dim As String answer
    ' convert a0, b0, c0 and d0 in hex, then add, low order first
    Dim As String s1 = Hex(a0, 8)
    For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
    s1 = Hex(b0, 8)
    For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
    s1 = Hex(c0, 8)
    For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next
    s1 = Hex(d0, 8)
    For i = 7 To 1 Step -2 : answer +=Mid(s1, i, 2) : Next

    Return LCase(answer)

End Function
