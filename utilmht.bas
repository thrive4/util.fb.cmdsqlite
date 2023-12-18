
Function replaceimageanchor(haystack As String, needle As String) As Integer
' based on https://rosettacode.org/wiki/Count_occurrences_of_a_substring#FreeBASIC
  If haystack = "" OrElse needle = "" Then Return 0
  Dim As Integer count = 0, length = Len(needle)
  dim dummy as string  
  For i As Integer = 1 To Len(haystack)
    If Mid(haystack, i, length) = needle Then
    dummy = Mid(haystack, i, (instr(i, haystack, ".") + 4) - i)
    haystack = replace(haystack, Mid(haystack, i, (instr(i, haystack, ".") + 4) - i), chr$(34) + mid(dummy, instrrev(dummy, "/") + 1))
      count += 1
      i += length - 1
    End If
  Next
  Return count
End Function

Sub Split(array() As String, text As String, wrapchar As String = " ")
    Dim As Integer bpos, epos, toks
    Dim As String tok
 
    Redim array(toks)
 
    Do While Strptr(text)
        epos = Instr(bpos + 1, text, wrapchar)
        array(toks) = Mid(text, bpos + 1, epos - bpos - 1)
        If epos = FALSE Then Exit Do      
        toks += 1
        Redim Preserve array(toks)
        bpos = epos
    Loop
End Sub

' decode a base64 encoded file
function mhtconvert(filename as string) as boolean
 
    ' init mht image or file input
    dim itemnr          as integer = 1
    dim listitem        as string
    dim i               as integer = 1
    ' init mht text
    dim chkcontenttype  as boolean = false
    dim tempfolder      as string
    dim orgname         as string
    dim textfile        as string
    Dim msg64           As String 
    dim textitem        as string
    dim chkhtml         as boolean = false
    dim linelength      as integer = 72

    tempfolder = mid(filename, instrrev(filename, "\"))
    tempfolder = exepath + mid(tempfolder, 1, instrrev(tempfolder, ".") - 1)

    if mkdir(tempfolder) < 0  then
        logentry("fatal", "error: could not create folder " + tempfolder)
    else
        print "exporting " + filename + " as html and text to " + tempfolder
    end if

    msg64 = ""
    textitem = ""
    orgname = mid(filename, instrrev(filename, "\") + 1)
    orgname = left(orgname, len(orgname) - 4) + ".html"
    textfile = tempfolder + "\" + orgname

    Open filename For input As 1
    open textfile for output as 3    

    Do Until EOF(1)
        ' stop decoding
        Line Input #1, listitem
        ' special case remove %2520 used in filenames images
        listitem = Replace(listitem, "%2520", "")
        ' filter out mht header for html
        if instr(listitem, "<html") = 0 and chkhtml = false then
            listitem = ""
        else
            chkhtml = true
        end if
        if instr(listitem, "------=_NextPart") > 0 then
            Print #2, base64decode(msg64)
            chkcontenttype = false
            msg64 = ""
            close (2)
        end if
        ' start decoding
        select case true
            case instr(listitem, "Content-Type: image") > 0
                chkcontenttype = true
            case instr(listitem, "Content-Type: text/javascript") > 0
                chkcontenttype = true
            case instr(listitem, "Content-Type: text/css") > 0
                chkcontenttype = true
            case instr(listitem, "Content-Type: font") > 0
                chkcontenttype = true
        end select
        if chkcontenttype then
            if instr(listitem, "Content-Location:") > 0 then
                ' output decoded images to a temp dir
                open tempfolder + "\" + mid(listitem, instrrev(listitem, "/") + 1) for output as 2
            end if
            ' ghetto validation base64
            select case true
                case instr(listitem, " ") > 0
                    'nop
                case instr(listitem, "-") > 0
                    'nop
                case instr(listitem, ":") > 0
                    'nop
                case instr(listitem, "%") > 0
                    'nop
                case len(listitem) = 0
                    'nop
                case else
                    msg64 = msg64 + listitem
            end select
        end if
        if chkcontenttype = false then
            select case true
                case instr(listitem, "------=_NextPart") > 0
                    listitem = ""
                case instr(listitem, "Content-Type:") > 0
                    listitem = ""
                case instr(listitem, "Content-Transfer-Encoding:") > 0
                    listitem = ""
                case instr(listitem, "Content-Location:") > 0
                    listitem = ""
            end select
            ' special cases mht
            ' remove frontpage thing sticks = to end of line
            if mid(listitem, len(listitem)) = "=" then
                listitem = mid(listitem, 1, len(listitem) - 1)
            end if
            textitem = textitem + listitem
        end if    
        itemnr += 1
    Loop

    ' generic replace for text and html
    textitem = htmlcleanup(textitem)
    print "nr image anchors changed: " & replaceimageanchor(textitem, chr$(34) + "file:///")
    print #3, textitem
    close

    return true

end function    
