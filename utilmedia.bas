csv         = "'filename', 'orientation', 'width', 'height', 'filesize', 'thumbnail'" + chr$(13) + chr$(10)
report      = ""
thumbnail   = ""
thumb       = 0
orientation = "sqare"

' attempt to extract and write cover art of mp3 to temp thumb file
Function imagemetric(filename as string, buffer As String) As boolean

    dim length  as string
    dim bend    as integer
    dim ext     as string = ""
    dim image   as string
    dim temp    as string
    thumbnail   = ""
    chunk       = ""
    report      = ""
    thumb       = 0
    orientation = "square"
    if instr(1, buffer, "APIC") > 0 and instr(filename, ".mp3") > 0 then
        length = mid(buffer, instr(buffer, "APIC") + 4, 4)
        ' ghetto check funky first 4 bytes signifying length image
        ' not sure how reliable this info is
        ' see comment codecaster https://stackoverflow.com/questions/47882569/id3v2-tag-issue-with-apic-in-c-net
        if val(asc(length, 1) & asc(length, 2)) = 0 then
            bend = (asc(length, 3) shl 8) or asc(length, 4)
        else
            bend = (asc(length, 1) shl 24 + asc(length, 2) shl 16 + asc(length, 3) shl 8 or asc(length, 4))
        end if
    end if
    ' get image dimensions jpg
    ' aided by https://www.freebasic.net/forum/viewtopic.php?t=21922&hilit=instr+hex+search&start=15
    ' and https://stackoverflow.com/questions/18264357/how-to-get-the-width-height-of-jpeg-file-without-using-library
    if instr(1, buffer, "JFIF") > 0 then
        ' override end jpg if marker FFD9 is present
        if instr(buffer, CHR(&hFF, &hD9)) > 0 then
            bend = instr(1, mid(buffer, instr(1, buffer, "JFIF")), CHR(&hFF, &hD9)) + 7
        end if
        chunk = mid(buffer, instr(buffer, "JFIF") - 6, bend)
        ' thumbnail detection
        if instr(instr(1, buffer, "JFIF") + 4, buffer, "JFIF") > 0 then
            thumbnail = thumbnail + "thumbnail in " + filename + chr$(13) + chr$(10)
            thumb = 1
            chunk = mid(buffer, instr(10, buffer, CHR(&hFF, &hD8)), instr(instr(buffer, CHR(&hFF, &hD9)) + 1, buffer, CHR(&hFF, &hD9)) - (instr(10, buffer, CHR(&hFF, &hD8)) - 2))
            ' thumbnail in thumbnail edge case ffd8 ffd8 ffd9 ffd9 pattern in jpeg
            if instr(chunk, CHR(&hFF, &hD8, &hFF)) > 0 then
                chunk = mid(buffer,_
                instr(1,buffer, CHR(&hFF, &hD8)),_
                instr(instr(instr(instr(1,buffer, CHR(&hFF, &hD9)) + 1, buffer, CHR(&hFF, &hD9)) + 1, buffer, CHR(&hFF, &hD9))_
                , buffer, CHR(&hFF, &hD9)) + 2 - instr(buffer, CHR(&hFF, &hD8)))
            end if
        end if
        if instr(chunk, CHR(&hFF, &hC2)) > 0 then
            coverwidth  = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 7, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 8, 1)))
            coverheight = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 5, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 6, 1)))
        else
            coverwidth  = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 7, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 8, 1)))
            coverheight = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 5, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 6, 1)))
        end if
        ext = ".jpg"
    end if
    ' use ext and exif check to catch false png
    if instr(1, buffer, "‰PNG") > 0 and instr(1, buffer, "Exif") = 0 and ext = "" then
        ' override end png if tag is present
        if instr(1, buffer, "IEND") > 0 then
            bend = instr(1, mid(buffer, instr(1, buffer, "‰PNG")), "IEND") + 7
        end if
        chunk = mid(buffer, instr(buffer, "‰PNG"), bend)
        ' get image dimensions png
        ' aided by see post by Ry- https://stackoverflow.com/questions/15327959/get-height-and-width-dimensions-from-base64-png
        ' and https://www.w3.org/TR/PNG-Chunks.html
        ' width
        length = mid(chunk, instr(chunk, "IHDR") + 4, 4)
        if val(asc(length, 1) & asc(length, 2)) = 0 then
            coverwidth  = cint("&H" + hex(asc(length, 3)) & hex(asc(length, 4)))
        else
            coverwidth  = cint("&H" + hex(asc(length, 1)) & hex(asc(length, 2)) & hex(asc(length, 3)) & hex(asc(length, 4)))
        end if
        ' height
        length = mid(chunk, instr(chunk, "IHDR") + 8, 4)
        if val(asc(length, 1) & asc(length, 2)) = 0 then
            coverheight = cint("&H" + hex(asc(length, 3)) & hex(asc(length, 4)))
        else
            coverheight = cint("&H" + hex(asc(length, 1)) & hex(asc(length, 2)) & hex(asc(length, 3)) & hex(asc(length, 4)))
        end if
        ext = ".png"
    end if
    ' funky variant for non jfif and jpegs video encoding?
    if (instr(1, buffer, "Lavc58") > 0 or instr(1, buffer, "Exif") > 0) and ext = "" then
        ' override end jpg if marker FFD9 is present
        if instr(buffer, CHR(&hFF, &hD9)) > 0 then
            bend = instr(1, mid(buffer, instr(1, buffer, "Exif")), CHR(&hFF, &hD9)) + 7
        end if
        if instr(1, buffer, "Exif") > 0 then
            chunk = mid(buffer, instr(buffer, "Exif") - 6, bend)
        else
            chunk = mid(buffer, instr(buffer, "Lavc58") - 6, bend)
        end if
        if instr(chunk, CHR(&hFF, &hC2)) > 0 then
            coverwidth  = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 7, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 8, 1)))
            coverheight = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 5, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 6, 1)))
        else
            coverwidth  = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 7, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 8, 1)))
            coverheight = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 5, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 6, 1)))
        end if
        ext = ".jpg"
    end if
    ' last resort just check on begin and end marker very tricky...
    ' see https://stackoverflow.com/questions/4585527/detect-end-of-file-for-jpg-images#4614629
    if instr(buffer, CHR(&hFF, &hD8)) > 0 and ext = "" then
        chunk = mid(buffer, instr(1, buffer, CHR(&hFF, &hD8)), instr(1, buffer, CHR(&hFF, &hD9)))
        ext = ".jpg"
        if instr(chunk, CHR(&hFF, &hC2)) > 0 then
            coverwidth  = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 7, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 8, 1)))
            coverheight = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 5, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC2)) + 6, 1)))
        else
            coverwidth  = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 7, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 8, 1)))
            coverheight = ((asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 5, 1)) shl 8) or asc(mid(chunk, instr(chunk, CHR(&hFF, &hC0)) + 6, 1)))
        end if
    end if
    ' report check for square layout with tolerance
    if coverwidth > 0 and coverheight > 0 then
        select case coverwidth / coverheight
            case is > 1.1
                layout = layout + "coverart not square " + "w: " & coverwidth  & " / h: " & coverheight & " - " & filename + chr$(13) + chr$(10)
                orientation = "landscape"
            case is < 0.9
                layout = layout + "coverart not square " + "w: " & coverwidth  & " / h: " & coverheight & " - " & filename + chr$(13) + chr$(10)
                orientation = "portrait"
        end select
        'print filename + "w" & coverwidth & " h" & coverheight & " ratio " & coverwidth / coverheight
    end if
    ' attempt to write mp3 coverart to temp file
    if instr(1, buffer, "APIC") > 0 and instr(filename, ".mp3") > 0 then
        temp = lcase(mid(filename, instrrev(filename, "\") + 1))
        temp =  lcase(mid(temp, 1, instr(temp, ".") - 1))
        if ext <> "" then
            image = exepath + "\cover\" + temp + ext
            open image for Binary Access Write as #1
                put #1, , chunk
            close #1
        else
            ' optional use folder.jpg if present as thumb
        end if
    end if
    buffer = ""

    return true

end function

' attempt to extract and write cover art of mp3 to temp thumb file
Function getmp3cover(filename As String, temp as string) As boolean

    Dim buffer  As String

    Open filename For Binary Access Read As #1
        If LOF(1) > 0 Then
            buffer = String(LOF(1), 0)
            Get #1, , buffer
        End If
    Close #1
    imagemetric(filename, buffer)
    report = report + "w: " & coverwidth
    report = report + " / h: " & coverheight
    report = report + " - " + filename
    csv = csv + chr$(34) + filename + chr$(34) + "," & orientation & "," & coverwidth & "," & coverheight & "," & len(chunk) & "," & thumb & chr(13) + chr$(10)
    print report

    return true

end function

' attempt to extract and write cover art of mp3 to temp thumb file
Function getimagemetric(filename As String) As boolean

    Dim buffer  As String

    Open filename For Binary Access Read As #1
        If LOF(1) > 0 Then
            buffer = String(LOF(1), 0)
            Get #1, , buffer
        End If
    Close #1
    imagemetric(filename, buffer)
'    report = report + "w: " & coverwidth
'    report = report + " / h: " & coverheight
'    report = report + " - " + filename
'    csv = csv + chr$(34) + filename + chr$(34) + "," & orientation & "," & coverwidth & "," & coverheight & "," & filelen(filename) & "," & thumb & chr(13) + chr$(10)
'    print report

    return true

end function

' parse .srt file
function srt2sql(filename As String, srtData() As String, startTime() As String, endTime() As String, tbname as string = "", tabletype as string = "") as uinteger
    Dim As UInteger x = 0 ' counter
    Dim As String text
    Dim As String dummy = ""
    Dim As String startTimeStr, endTimeStr
    
    Dim As long f
    f = FreeFile
    Open filename For Input As #f

    Do While Not EOF(f)
        Line Input #f, text
        If Len(text) > 0 Then
            ' check start and end time
            If InStr(text, " --> ") > 0 Then
                ' split the line into start and end time
                startTimeStr = Left(text, InStr(text, " --> ") - 1)
                endTimeStr = Mid(text, InStr(text, " --> ") + 5)
            Else
                ' append the line to the current block
                dummy &= text + "|"
            End If
        Else
            ' end of a subtitle block, add it to the array
            ReDim Preserve srtData(x) As String
            ReDim Preserve startTime(x) As String
            ReDim Preserve endTime(x) As String
            srtData(x) = dummy
            startTime(x) = startTimeStr
            endTime(x) = endTimeStr
            dummy = ""
            x += 1
        End If

    Loop
    Close #f

    return x

end function
