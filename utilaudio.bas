
' file type specific functions
' ______________________________________________________________________________'

' code by squall4226
' see https://www.freebasic.net/forum/viewtopic.php?p=149207&hilit=user+need+TALB+for+album#p149207
Function getmp3tag(searchtag As String, fn As String) As String
   'so we can avoid having the user need TALB for album, TIT2 for title etc, although they are accepted
   Dim As Integer skip, offset' in order to read certain things right
   ' reduce iterations with maxcheck tricky could miss data 
   Dim As UInteger sig_to_find, count, maxcheck = 3000
   'Dim As UInteger sig_to_find, count, maxcheck = 100000
   dim as long fnum
   dim as UShort tag_length
   Dim As UShort unitest, mp3frametest
   Dim As String tagdata

   Select Case UCase(searchtag)
        Case "HEADER", "ID3"
            searchtag = "ID3" & Chr(&h03)
        Case "TITLE", "TIT2"
            searchtag = "TIT2"
        Case "ARTIST", "TPE1"
            searchtag = "TPE1"
        Case "ALBUM", "TALB"
            searchtag = "TALB"
        Case "COMMENT", "COMM"
            searchtag = "COMM"
        Case "COPYRIGHT", "TCOP"
            searchtag = "TCOP"
        Case "COMPOSER", "TCOM"
            searchtag = "TCOM"
        Case "BEATS PER MINUTE", "BPM", "TPBM"
            searchtag = "TBPM"
        Case "PUBLISHER", "TPUB"
            searchtag = "TPUB"
        Case "URL", "WXXX"
            searchtag = "WXXX"
        Case "PLAY COUNT" "PCNT"
            searchtag = "PCNT"
        Case "GENRE", "TCON"
            searchtag = "TCON"
        Case "ENCODER", "TENC"
            searchtag = "TENC"
        Case "TRACK", "TRACK NUMBER", "TRCK"
            searchtag = "TRCK"
        Case "YEAR", "TYER"
            searchtag = "TYER"      
        'Special, in this case we will return the datasize if present, or "-1" if no art
        Case "PICTURE", "APIC"
            searchtag = "APIC"
            'Not implemented yet!
        Case Else
            'Tag may be invalid, but search anyway, there are MANY tags, and we have error checking
   End Select

   fnum = FreeFile
   Open fn For Binary Access Read As #fnum
   If Lof(fnum) < maxcheck Then maxcheck = Lof(fnum)
   For count = 0 to maxcheck Step 1
        Get #fnum, count, sig_to_find
        If sig_to_find = Cvi(searchtag) Then
             If searchtag = "ID3" & Chr(&h03) Then
                Close #fnum
                Return "1" 'Because there is no data here, we were just checking for the ID3 header
             EndIf
             'test for unicode
             Get #fnum, count+11, unitest         
             If unitest = &hFEFF Then 'unicode string
                skip = 4
                offset = 13           
             Else 'not unicode string
                skip = 0
                offset = 10            
             EndIf
             
             Get #fnum, count +7, tag_length 'XXXXYYYZZ Where XXXX is the TAG, YYY is flags or something, ZZ is size

             If tag_length-skip < 1 Then
                Close #fnum
                Return "ERROR" 'In case of bad things
             EndIf
             
             Dim As Byte dataget(1 To tag_length-skip)
             Get #fnum, count+offset, dataget()
             
             For i As Integer = 1 To tag_length - skip
                if dataget(i) < 4 then dataget(i) = 0 ' remove odd characters
                If dataget(i) <> 0 Then tagdata + = Chr(dataget(i)) 'remove null spaces from ASCII data in UNICODE string
             Next
        End If
        If tagdata <> "" then exit For ' stop searching!
   Next
   Close #fnum
   
   If Len(tagdata) = 0 Then
        'If the tag was just not found or had no data then "----"
        tagdata = "----"
   EndIf

   Return tagdata

End Function

' get base mp3 info
'dim shared taginfo(1 to 5) as string
function getmp3baseinfo(fx1File as string) as boolean
    taginfo(1) = getmp3tag("artist",fx1File)
    taginfo(2) = getmp3tag("title", fx1File)
    taginfo(3) = getmp3tag("album", fx1File)
    taginfo(4) = getmp3tag("year",  fx1File)
    taginfo(5) = getmp3tag("genre", fx1File)
    ' use last part path as theme
    ReDim As String ordinance(0)
    explode(fx1File, "\", ordinance())
    taginfo(6) = ordinance(UBound(ordinance) -1)

    if taginfo(1) <> "----" and taginfo(2) <> "----" then
        'nop
    else    
        taginfo(1) = mid(left(fx1File, len(fx1File) - instr(fx1File, "\") -1), InStrRev(fx1File, "\") + 1, len(fx1File))
        taginfo(2) = ""
    end if                
    return true
end function

function getmp3playlist(filename as string, listname as string) as integer
    dim              as long f, g, h
    dim itemnr       as integer = 1
    dim listitem     as string
    dim listduration as integer
    dim mp3listtype  as string = ""
    f = freefile

    select case true  
        case instr(filename, ".pls") > 0
            mp3listtype = "pls"
        case instr(filename, ".m3u") > 0
            mp3listtype = "m3u"
        case else
            return 0
    end select    

    if len(filename) = 0 then
        logentry("warning", filename + " path or file not found.")
    else
        logentry("notice", "parsing and playing plylist " + filename)
    end if
    Open filename For input As #f
    g = freefile
    open exepath + "\" + listname + ".tmp" for output as #g
    h = freefile
    open exepath + "\" + listname + ".lst" for output as #h
    itemnr = 0

    Do Until EOF(f)
        Line Input #f, listitem
        ' ghetto parsing pls
        if mp3listtype = "pls" then
            if instr(listitem, "=") > 0 then
                select case true
                    case instr(listitem, "file") > 0
                        print #g, mid(listitem, instr(listitem, "=") + 1, len(listitem))
                        print #h, mid(listitem, instr(listitem, "=") + 1, len(listitem))
                        itemnr += 1
                    case instr(listitem, "title" + str(itemnr)) > 0
                    case instr(listitem, "length" + str(itemnr)) > 0
                        listduration = listduration + val(mid(listitem, instr(listitem, "=") + 1, len(listitem)))
                    case len(listitem) = 0
                        'nop
                    case else
                        'msg64 = msg64 + listitem
                end select
            end if
        end if
        ' ghetto parsing m3u
        if mp3listtype = "m3u" then
            ' ghetto parsing m3u
            if len(listitem) > 0 then
                select case true
                    case instr(listitem, "EXTINF:") > 0
                        listduration = listduration + val(mid(listitem, instr(listitem, ":") + 1, len(instr(listitem, ","))- 1))
                    case instr(listitem, ".") > 0
                        print #g, listitem
                        print #h, listitem
                        itemnr += 1
                    case len(listitem) = 0
                        'nop
                    case else
                        'msg64 = msg64 + listitem
                end select
            end if
        end if
    Loop
    'maxmusicitems = itemnr
    close(f)
    close(g)
    close(h)
    return itemnr

end function

' export m3u
' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function exportm3u(folder as string, filterext as string, listtype as string = "m3u", htmloutput as string = "default", tag as string = "", tagquery as string = "") as integer
    ' setup filelist
    dim                as integer i = 1, j=1, n = 1, attrib, itemnr, maxfiles
    dim                as long tmp
    dim dummy          as string
    dim dummy2         as string
    dim tbname         as string
    dim file           as string
    dim fileext        as string
    dim fsize          as long
    dim fdate          as string
    dim fattr          as string
    dim argc(0 to 5)   as string
    dim argv(0 to 5)   as string

    redim path(1 to 1) As string
    'export to m3u
    Open exepath + "\" + tagquery + ".m3u" For output As #20
    print #20, "#EXTM3U"
    print "exporting result to " & exepath + "\" + tagquery + ".m3u"  & " ..."

    #ifdef __FB_LINUX__
      const pathchar = "/"
    #else
      const pathchar = "\"
    #endif
    ' read dir recursive starting directory
    path(1) = folder 
    if( right(path(1), 1) <> pathchar) then
        file = dir(path(1), fbNormal or fbDirectory, @attrib)
        if( attrib and fbDirectory ) then
            path(1) += pathchar
        end if
    end if

    recnr = 0
    cls
    while i <= n
    file = dir(path(i) + "*" , fbNormal or fbDirectory, @attrib)
        while file > ""
            if (attrib and fbDirectory) then
                if file <> "." and file <> ".." then
                    n += 1
                    redim preserve path(1 to n)
                    path(n) = path(i) + file + pathchar
                end if
            else
                fileext = lcase(mid(file, instrrev(file, ".")))
                if instr(1, filterext, fileext) > 0 and len(fileext) > 3 then
                    ' get specific file information
                    fsize = filelen(path(i) + file)
                    fdate = Format(FileDateTime(path(i) + file), "yyyy-mm-dd hh:mm:ss" )
                    If (attrib And fbReadOnly) <> 0 Then fattr = "read-only"
                    If (attrib And fbHidden  ) <> 0 Then fattr = "hidden"
                    If (attrib And fbSystem  ) <> 0 Then fattr = "system"
                    If (attrib And fbArchive ) <> 0 Then fattr = "archived"
                    select case listtype
                        case "m3u"
                            if instr(filterext, ".mp3") > 0 and htmloutput = "exif" then
                                Locate 1, 1   
                                print "scanning " & folder + " with filespec " + filterext + " with tag " & tag & " contains " & tagquery
                                print str(recnr)
                                recnr += 1
                                ' path(i) folder and drive
                                getmp3baseinfo(path(i) + file)
                                argc(0) = "artist"
                                argc(1) = "title"
                                argc(2) = "album"
                                argc(3) = "year"
                                argc(4) = "genre"
                                argc(5) = "nop"

                                argv(0) = taginfo(1)
                                argv(1) = taginfo(2)
                                argv(2) = taginfo(3)
                                argv(3) = taginfo(4)
                                argv(4) = taginfo(5)
                                argv(5) = "nop"
                            end if

                            For j As Integer = 0 To 5
                                'export to m3u
                                if argc(j) = tag and instr(lcase(argv(j)), lcase(tagquery)) > 0 then
                                    print #20, "#EXTINF:134," & argv(0) & " - " & argv(1)
                                    print #20, path(i) & file
                                    maxfiles += 1
                                end if
                            Next j
                    end select
                else
                    'logentry("warning", "file format not supported - " + path(i) & file)
                end if    
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend

    recnr = recnr - 1
    print "scanned " & recnr & " files in " + folder + " with filespec " + filterext + " " & maxfiles & " file(s) found with " & tag & " " & tagquery
    logentry("notice", "scanned and exported m3u")
    close(20)
    return maxfiles

end function
