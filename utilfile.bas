' used for app launcher
#include once "crt/process.bi"
' dir function and provides constants to use for the attrib_mask parameter
#include once "vbcompat.bi"
#include once "dir.bi"

' setup word counter
type wordtally
    as string  word(any)
    as integer count(any)
end type
dim shared wc as wordtally
common shared wclinenr as integer
wclinenr = 0
declare function arrayhighestvalue(needle as string, wc as wordtally) as integer
declare function dictonary(filename as string, wc as wordtally) as string

' setup log
dim shared logfile    as string
dim shared logtype    as string
dim shared appname    as string
dim shared appfile    as string
dim shared usecons    as string
dim shared exeversion as string
dim shared taginfo(1 to 6) as string

' note command(0) can arbitraly add the path so strip it
appname = mid(command(0), instrrev(command(0), "\") + 1)
' without file extension
if instr(appname, ".exe") > 0 then
    appname = left(appname, len(appname) - 4)
end if
' options logtype verbose, full
logtype = "verbose"
' options usecons true, false
usecons = "false"
' generic check for true or false
dim chk as boolean

' get version exe for log
declare function getfileversion(versinfo() as string, versdesc() as string) as integer
declare function replace(byref haystack as string, byref needle as string, byref substitute as string) as string
declare Function explode(haystack As String = "", delimiter as string, ordinance() As String) As UInteger
declare function getmp3baseinfo(fx1File as string) as boolean
dim as integer c, resp
dim as string versinfo(8)
dim as string versdesc(7) =>_
    {"CompanyName",_
    "FileDescription",_
    "FileVersion",_
    "InternalName",_
    "LegalCopyright",_
    "OriginalFilename",_
    "ProductName",_
    "ProductVersion"}
versinfo(8) = appname + ".exe"
resp = getfileversion(versinfo(),versdesc())
exeversion = replace(trim(versinfo(2)), ", ", ".")

' get metric os
dim shared os as string
os = "unknown"
#ifdef __FB_WIN32__
    os = "windows"
#endif
#ifdef __FB_UNIX__
    os = "unix"
#endif

' metric functions
' ______________________________________________________________________________'

' used for logging
' entrytypes: error, fatal, notice, warning, terminate
Function logentry(entrytype As String, logmsg As String) As Boolean

    ' validate logentry
    If InStr(logmsg, "|") > 0 Then
        logmsg = "entry contained delimeter -> | <-"
    End If

    ' output to console
    if usecons = "true" then
        print time & " " + entrytype + " | " + logmsg
    end if

    ' setup logfile
    dim f as long
    f = FreeFile
    logfile = exepath + "\" + appname + ".log"
    if FileExists(logfile) = false then
        Open logfile For output As #f
        print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + "notice" + "|" + appname + "|" + logfile + " created"
        print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + "notice" + "|" + appname + "|" + "version " + exeversion
        print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + "notice" + "|" + appname + "|" + "platform " + os
        close #f
    end if

    if (entrytype = "warning" or entrytype = "notice") and logtype = "verbose" then
        return true
    end if

    ' write to logfile
    Open logfile For append As #f
    print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + entrytype + "|" + appname + "|" + logmsg
    close #f

    ' normal termination or fatal error
    select case entrytype
        case "fatal"
            print logmsg
            end
        case "terminate"
            end
    end select

    return true
End function

' get fileversion executable or dll windows only
function getfileversion(versinfo() as string, versdesc() as string) as integer

    dim as integer bytesread,c,dwHandle,res,verSize
    dim as string buffer,ls,qs,tfn
    dim as ushort ptr b1,b2
    dim as ubyte ptr bptr

    tfn=versinfo(8)
    if dir(tfn)="" then return -1
    verSize=GetFileVersionInfoSize(tfn,@dwHandle)
    if verSize=0 then return -2
    dim as any ptr verdat=callocate(verSize*2)

    res=GetFileVersionInfo(strptr(tfn),dwHandle,verSize*2,verdat)
    res=_
        VerQueryValue(_
            verdat,_
            "\VarFileInfo\Translation",_
            @bptr,_
            @bytesread)

    if bytesread=0 then deallocate(verdat):return -3

    b1=cast(ushort ptr,bptr)
    b2=cast(ushort ptr,bptr+2)
    ls=hex(*b1,4)& hex(*b2,4)

    for c=0 to 7
        qs="\StringFileInfo\" & ls & "\" & versdesc(c)
        res=_
            VerQueryValue(_
                verdat,_
                strptr(qs),_
                @bptr,_
                @bytesread)
        if bytesread>0 then
            buffer=space(bytesread)
            CopyMemory(strptr(buffer),bptr,bytesread)
            versinfo(c)=buffer
        else
            versinfo(c)="N/A"
        end if
    next c
    deallocate(verdat)

    return 1

end function

' process timer
type execduration
    as double beginexectime
    as double endexectime
end type
dim exectimer as execduration

function exectime(exectimer as execduration, state as string) as string

    select case state
        case "set"
            exectimer.beginexectime = timer
            return "true"
        case "stop"
            exectimer.endexectime = timer - exectimer.beginexectime
            return format((exectimer.endexectime) * 1000, "00:000") + " (ms)"
    end select

end function

' generic file functions
' ______________________________________________________________________________'


' sample code for calling the function getfolders and getfilesfromfolder
'ReDim As String ordinance(0)
'getfilesfromfolder("i:\games\*", ordinance())
'print UBound(ordinance)
'For x As Integer = 1 To UBound(ordinance)
'    Print ordinance(x)
'Next

' list files in folder
function getfilesfromfolder(filespec As String, ordinance() As String) as uinteger
    Dim As UInteger x      = 0 'counter
    Dim As String filename = Dir(filespec, fbnormal, fbHidden and fbSystem and fbArchive and fbReadOnly)

    if len(filename) = 0 then print "path not found..." end if
    Do While Len(filename) > 0
        x += 1
        ReDim Preserve ordinance(x) 'create new array element
        ordinance(x) = filename
        filename = Dir()
    Loop

    return x

end function

' list folders
function getfolders (filespec As String, ordinance() As String) as uinteger
    Dim As UInteger x = 0 'counter
    var mask          = fbDirectory or fbHidden or fbSystem or fbArchive or fbReadOnly
    var attrib        = 0
    var filename      = dir( filespec, mask, attrib )
    
    if len(filename) = 0 then print "path not found..." end if
    ' show directory regardless if it is system, hidden, read-only, or archive
    while(filename > "")
        if(attrib and fbDirectory) and (filename <> "." and filename <> "..") then
            x += 1
            ReDim Preserve ordinance(x) 'create new array element
            ordinance(x) = filename
        end if
        filename= dir(attrib)
    wend

    return x

end function

function getdrivelabel(drive as string) as string
    Dim As ZString * 1024 deviceName
    Dim As ZString * 1024 volumeName
    QueryDosDevice(drive, deviceName, 1024)
    GetVolumeInformation(drive, volumeName, 1024, 0, 0, 0, 0, 0)
    return volumeName
end function

function getdrivestorage(drive as string, metric as string) as ULongInt
    Dim As ULARGE_INTEGER freeBytesAvailable
    Dim As ULARGE_INTEGER totalNumberOfBytes
    Dim As ULARGE_INTEGER totalNumberOfFreeBytes
    If GetDiskFreeSpaceEx(drive, @freeBytesAvailable, @totalNumberOfBytes, @totalNumberOfFreeBytes) Then
        select case metric
            case "capacity"
                return totalNumberOfBytes.QuadPart
            case "space"
                return totalNumberOfFreeBytes.QuadPart
            case else
                return 0
        end select
    Else
    '    Print "Error: "; GetLastError()
        return 0
    End If
end function

function convertbytesize(totalsize as longint) as string

    dim size as string
    if totalsize < 1024 then
        size = str(totalsize) & " bytes"
    elseif totalsize < 1048576 then
        size = format(totalsize / 1024.0, "0.00") & " KB"
    elseif totalsize < 1073741824 then
        size = format(totalsize / 1048576.0, "0.00") & " MB"
    else
        size = format(totalsize / 1073741824.0, "0.00") & " GB"
    end if

    return size

end function

' get folder size
function foldersize(folder as string) as longint

    redim path(1 to 1) As string
    dim file           as string
    dim fileext        as string
    dim maxfiles       as integer
    dim totalsize      as longint = 0 
    dim as integer i = 1, n = 1, attrib

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

    while i <= n
    file = dir(path(i) + "*" , fbNormal or fbDirectory, @attrib)
        while file > ""
            if (attrib and fbDirectory) then
                if file <> "." and file <> ".." then
                    ' limit recursive if starting folder is root
                    'if len(path(1)) > 3 then
                        n += 1
                        redim preserve path(1 to n)
                        path(n) = path(i) + file + pathchar
                    'end if
                end if
            else
                fileext = lcase(mid(file, instrrev(file, ".")))
                    totalsize += filelen(path(i) & file)
                    maxfiles += 1
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend

    return totalsize

end function

' create a new file
Function newfile(filename As String) As boolean
    Dim f As long

    if FileExists(filename) then
        logentry("fatal", "creating " + filename + " file excists")
        return false
    end if    

    f = FreeFile
    Open filename For output As #f
    logentry("notice", filename + " created")
    close(f)
    return true

End Function

' create a temp file
Function tmp2file(filename As String) As boolean
    Dim f As long

    if FileExists(filename) = true then
      If Kill(filename) <> 0 Then
          logentry("warning", "could not delete " + filename )
      end if
    end if

    f = FreeFile
    Open filename For output As #f
    logentry("notice", filename + " created")
    close(f)
    return true

End Function

' append to an excisiting file
Function appendfile(filename As String, msg as string) As boolean
    Dim f As long

    if FileExists(filename) = false then
        logentry("fatal", "appending " + filename + " file does not excist")
        return false
    end if

    f = FreeFile
    Open filename For append As #f
    print #f, msg
    close(f)
    return true

End Function

' read a file
Function readfromfile(filename As String) As long
    Dim f As long

    if FileExists(filename) = false then
        logentry("fatal", "reading " + filename + " file does not excist")
    end if

    f = FreeFile
    Open filename For input As #f
    return f

End Function

' delete a file
Function delfile(filename As String) As boolean

    if FileExists(filename) = true then
        If Kill(filename) <> 0 Then
            logentry("warning", "could not delete " + filename)
            return false
        end if
    end if
    return true

End Function

' check path
Function checkpath(chkpath As String) As boolean

    dim dummy as string
    dummy = curdir

    if chdir(chkpath) <> 0 then
        logentry("warning", "path " + chkpath + " not found")
        chdir(dummy)
        return false
    end if

    chdir(dummy)
    return true

End Function

' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function createlist(folder as string, filterext as string, listname as string) as integer
    ' setup filelist
    dim chk            as boolean
    redim path(1 to 1) As string
    dim as integer i = 1, n = 1, attrib
    dim as long f, g
    dim file           as string
    dim fileext        as string
    dim maxfiles       as integer
    f = freefile
    dim filelist as string = exepath + "\" + listname + ".tmp"
    open filelist for output as #f

    g = freefile
    dim filelistb as string = exepath + "\" + listname + ".lst"
    open filelistb for output as #g

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

    while i <= n
    file = dir(path(i) + "*" , fbNormal or fbDirectory, @attrib)
        while file > ""
            if (attrib and fbDirectory) then
                if file <> "." and file <> ".." then
                    ' todo evaluate limit recursive if starting folder is root
                    if len(path(1)) > 3 then
                        n += 1
                        redim preserve path(1 to n)
                        path(n) = path(i) + file + pathchar
                    else
                        logentry("error", "scanning from root dir not supported! " + path(i))
                    end if
                end if
            else
                fileext = lcase(mid(file, instrrev(file, ".")))
                if instr(1, filterext, fileext) > 0 and len(fileext) > 3 then 
                    print #f, path(i) & file
                    print #g, path(i) & file
                    maxfiles += 1
                else
                    logentry("warning", "file format not supported - " + path(i) & file)
                end if    
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend
    close(f)
    close(g)

    ' chk if filelist is created
    if FileExists(filelist) = false then
        logentry("warning", "could not create filelist: " + filelist)
    end if

    return maxfiles
end function

' localization file functions
' ______________________________________________________________________________'

' localization can be applied by getting a locale or other method
dim locale as string = "en"
sub displayhelp(locale as string)
    dim dummy as string
    dim f     as long
    f = freefile

    ' get text
    if FileExists(exepath + "\conf\" + locale + "\help.ini") then
        'nop
    else
        logentry("error", "open " + exepath + "\conf\" + locale + "\help.ini" + " file does not excist")
        locale = "en"
    end if
    Open exepath + "\conf\" + locale + "\help.ini" For input As #f
    Do Until EOF(f)
        Line Input #f, dummy
        ' hack issue with wstr and cmdline '| more' see ticket
        ' https://github.com/freebasic/fbc/issues/420
        if locale <> "en" and locale <> "nl" then
            print wstr(dummy)
        else
            print dummy
        end if
    Loop
    close f

end sub

' setup ui labels aka data spindel
type tbrec
    as string fieldname(any)
    as string fieldvalue(any)
end type
dim shared record as tbrec
common shared recnr as integer
recnr = 0

' get key value pair cheap localization via ini file
Function readuilabel(filename as string) as boolean
    dim itm    as string
    dim inikey as string
    dim inival as string
    dim f      as long

    if FileExists(filename) = false then
        logentry("error", filename + " does not excist switching to default language")
        filename = exepath + "\conf\en\menu.ini"
    end if
    f = readfromfile(filename)
    Do Until EOF(f)
        Line Input #f, itm
        if instr(1, itm, "=") > 1 then
            inikey = trim(mid(itm, 1, instr(1, itm, "=") - 2))
            inival = trim(mid(itm, instr(1, itm, "=") + 2, len(itm)))
            if inival = "" then
                logentry("error", inikey + " has empty value in " + filename)
                inival = "null"
            end if
            'print inikey + " - " + inival
            recnr += 1
            redim preserve record.fieldname(0 to recnr)
            redim preserve record.fieldvalue(0 to recnr)
            record.fieldname(recnr)  = inikey
            record.fieldvalue(recnr) = inival
        end if
    loop
    close f
    return true
end function

' display ui lable with unicode and semi automatic spacing via offset
function getuilabelvalue(needle as string, suffix as string = "", offset as integer = 10) as boolean
    dim fieldname  as string = ""
    dim fieldvalue as string = ""

    for i as integer = 0 to recnr
        with record
            if record.fieldname(i) = needle then
                fieldname  = record.fieldname(i)
                fieldvalue = record.fieldvalue(i)
            end if
        end with
    next i
    if fieldname = "" or fieldvalue = "" then
        print needle + " not found or empty " + suffix
        return false
    else
        print wstr(fieldvalue + space(offset - Len(fieldvalue)) + suffix)
        return true
    end if
end function

' file type specific functions
' ______________________________________________________________________________'

Dim Shared As String B64
B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" & _
"abcdefghijklmnopqrstuvwxyz" & _
"0123456789+/"

#define E0(v1) v1 Shr 2
#define E1(v1, v2) ((v1 And 3) Shl 4) + (v2 Shr 4)
#define E2(v2, v3) ((v2 And &H0F) Shl 2) + (v3 Shr 6)
#define E3(v3) (v3 And &H3F)

' via https://rosettacode.org/wiki/Base64_encode_data?section=1#FreeBASIC
Function base64encode(S As String) As String
    Dim As Integer j, k, l = Len(S)
    Dim As String  mE
    If l = 0 Then Return mE
    
    mE = String(((l+2)\3)*4,"=")
    For j = 0 To l - ((l Mod 3)+1) Step 3
        mE[k+0]=B64[e0(S[j+0])]
        mE[k+1]=B64[e1(S[j+0],S[j+1])]
        mE[k+2]=B64[e2(S[j+1],S[j+2])]
        mE[k+3]=B64[e3(S[j+2])]:k+=4
    Next j
    If (l Mod 3) = 2 Then
        mE[k+0]=B64[e0(S[j+0])]
        mE[k+1]=B64[e1(S[j+0],S[j+1])]
        mE[k+2]=B64[e2(S[j+1],S[j+2])]
        mE[k+3]=61
    Elseif (l Mod 3) = 1 Then
        mE[k+0]=B64[e0(S[j+0])]
        mE[k+1]=B64[e1(S[j+0],S[j+1])]
        mE[k+2]=61
        mE[k+3]=61
    End If
    Return mE
End Function

Function MIMEDecode(s As String ) As Integer
    If Len(s) Then
        MIMEdecode = Instr(B64,s) - 1
    Else
        MIMEdecode = -1
    End If
End Function

' via https://rosettacode.org/wiki/Base64_decode_data#FreeBASIC
Function base64decode(s As String) As String
    Dim As Integer w1, w2, w3, w4
    Dim As String  mD
    For n As Integer = 1 To Len(s) Step 4
        w1 = MIMEdecode(Mid(s,n+0,1))
        w2 = MIMEdecode(Mid(s,n+1,1))
        w3 = MIMEdecode(Mid(s,n+2,1))
        w4 = MIMEdecode(Mid(s,n+3,1))
        If w2 >-1 Then mD+= Chr(((w1* 4 + Int(w2/16)) And 255))
        If w3 >-1 Then mD+= Chr(((w2*16 + Int(w3/ 4)) And 255))
        If w4 >-1 Then mD+= Chr(((w3*64 + w4        ) And 255))
    Next n
    Return mD
End Function

' cheap text to sql export
Function txt2sql(filename as string, tbname as string = "", tabletype as string = "", inserttype as string = "complete") As boolean

    Dim f       As long
    Dim cnt     As integer = 0
    Dim fieldnr As integer = 0
    dim chk     as boolean = false
    dim dbchk   as boolean = false
    Dim text    As String
    dim dummy   as string = ""
    ' used for "... , ..." structure
    dim i       as integer = 1
    dim b       as integer = 0
    dim e       as integer = 0
    dim p       as string  = ""
    dim temp    as string

    ' filter out ext
    tbname = left(filename, instrrev(filename, ".") - 1)
    ' filter out preceding path if present
    tbname = lcase(mid(tbname, instrrev(tbname, "\") + 1))
    ' filter out space
    tbname = replace(tbname, " ", "")    

    if FileExists(filename) = false then
        logentry("fatal", "file not found or missing..'" & filename & "'")
    end if

    f = FreeFile
    Open filename For input As #f
    print "begin transaction;"
    if tabletype = "fts" then
        print "create virtual table if not exists '" + tbname + "' using fts5(";        
    else
        print "create table if not exists '" + tbname + "' ("
    end if

    Do Until EOF( f )
       Line Input #f, text
        ' create table defintion
        if cnt = 0 then
            ReDim As String ordinance(0)
            ' use strict cleaning for now
            text = replace(text, "'", "")
            text = replace(text, chr$(34), "")
            explode(text, "", ordinance())
            For x As Integer = 1 To UBound(ordinance)
                if tabletype = "fts" then
                '    if x <> UBound(ordinance) then
                '        ' rank is a reserved fieldname with fts5
                '        if lcase(trim(ordinance(x))) = "rank" then ordinance(x) = "ranked" end if
                '        Print "'" + lcase(trim(ordinance(x))) + "'," 
                '    else
                '        Print "'" + lcase(trim(ordinance(x))) + "'"
                '    end if
                    print
                    print "'text'"    
                else
                    if x <> UBound(ordinance) then
                        Print "'text'     text,"
                    else
                        Print "'text'     text"
                    end if
                end if
            Next
            fieldnr = UBound(ordinance)
            print ");"
        else
            ' create inserts   
            if inserttype = "complete" then
                dummy += text + chr$(10) + chr$(13)
                'dummy += text + "<br>"
            else
                dummy = ""
                ReDim As String ordinance(0)
                ' work around data "... , ..." structure still if-y...
                if instr(text, chr$(34)) > 0 then
                    temp = text
                    do
                        p = mid(text,i,1)
                        if p = chr$(34) and b > 0 and e = 0 then
                            e = i
                            dummy = mid(text, b, e - (b - 1))
                            dummy = replace(dummy, ",", "|\|")
                            temp = replace(temp,  mid(text, b, e - (b - 1)) , dummy)
                            dummy = ""
                            b = 0
                            p = ""
                        end if
                        if p = chr$(34) and b = 0 then
                            b = i
                            e = 0
                            p = ""
                        end if
                        i += 1
                    loop until i > len(text)
                    text = temp
                    ' reset workaround
                    b = 0 : e = 0 : i = 0 : p = "" : dummy = ""
                end if
                ' remove trailing comma at end of record
                if mid(text, len(text) - 1) = chr$(34) + "," then
                    text = mid(text, 1, len(text) - 1)
                end if
                explode(text, "", ordinance())
                if UBound(ordinance) <> fieldnr then
                    logentry("fatal", "error unequal amount of fields at line " & cnt + 1 & " " + replace(text, "|", ""))
                end if
                For x As Integer = 1 To UBound(ordinance)
                    ' restore data "... , ..." structure
                    'ordinance(x) = replace(trim(ordinance(x)), chr$(34), "")
                    'ordinance(x) = replace(trim(ordinance(x)), "|\|", ",")
                    ' check validty data 
                    if instr(trim(ordinance(x)), "'") > 0 then
                        logentry("warning", "found unescaped ' modified with '' at line " & cnt + 1 & " " + text)
                    end if    
                    if x <> UBound(ordinance) then
                        dummy += "'" + replace(trim(ordinance(x)), "'", "''") + "',"
                    else
                        dummy += "'" + replace(trim(ordinance(x)), "'", "''") + "'"
                    end if
                Next
                Print "insert into '" + tbname + "' values (" + dummy + ");" 
            end if
        end if
        cnt += 1
    Loop
    if inserttype = "complete" then
        dummy = replace(dummy, "'", "''")
        Print "insert into '" + tbname + "' values ('" + dummy + "');" 
    end if
    print "commit;"
    close(f)
    logentry("notice", "exported text " + filename + " to sql with tablename " + tbname + " #recs " & cnt)

    return true
    
end function

' cheap csv to sql export
Function csv2sql(filename as string, tbname as string = "", tabletype as string = "") As boolean

    Dim f       As long
    Dim cnt     As integer = 0
    Dim fieldnr As integer = 0
    dim chk     as boolean = false
    dim dbchk   as boolean = false
    Dim text    As String
    dim dummy   as string = ""
    ' used for "... , ..." structure
    dim i       as integer = 1
    dim b       as integer = 0
    dim e       as integer = 0
    dim p       as string  = ""
    dim temp    as string

    ' filter out ext
    tbname = left(filename, instrrev(filename, ".") - 1)
    ' filter out preceding path if present
    tbname = lcase(mid(tbname, instrrev(tbname, "\") + 1))

    if FileExists(filename) = false then
        logentry("fatal", "file not found or missing..'" & filename & "'")
    end if

    f = FreeFile
    Open filename For input As #f
    print "begin transaction;"
    if tabletype = "fts" then
        print "create virtual table if not exists '" + tbname + "' using fts5("        
    else
        print "create table if not exists '" + tbname + "' ("
    end if

    Do Until EOF( f )
       Line Input #f, text
        ' create table defintion
        if cnt = 0 then
            ReDim As String ordinance(0)
            ' use strict cleaning for now
            text = replace(text, "'", "")
            text = replace(text, chr$(34), "")
            explode(text, ",", ordinance())
            For x As Integer = 1 To UBound(ordinance)
                if tabletype = "fts" then
                    if x <> UBound(ordinance) then
                        ' rank is a reserved fieldname with fts5
                        if lcase(trim(ordinance(x))) = "rank" then ordinance(x) = "ranked" end if
                        Print "'" + lcase(trim(ordinance(x))) + "'," 
                    else
                        Print "'" + lcase(trim(ordinance(x))) + "'"
                    end if
                else
                    if x <> UBound(ordinance) then
                        Print "'" + lcase(trim(ordinance(x))) + "'" + space(20 - len(trim(ordinance(x)))) + "text" + "," 
                    else
                        Print "'" + lcase(trim(ordinance(x))) + "'" + space(20 - len(trim(ordinance(x)))) + "text" 
                    end if
                end if
            Next
            fieldnr = UBound(ordinance)
            print ");"
        else
            ' create inserts    
            dummy = ""
            ReDim As String ordinance(0)
            ' work around data "... , ..." structure still if-y...
            if instr(text, chr$(34)) > 0 then
                temp = text
                do
                    p = mid(text,i,1)
                    if p = chr$(34) and b > 0 and e = 0 then
                        e = i
                        dummy = mid(text, b, e - (b - 1))
                        dummy = replace(dummy, ",", "|\|")
                        temp = replace(temp,  mid(text, b, e - (b - 1)) , dummy)
                        dummy = ""
                        b = 0
                        p = ""
                    end if
                    if p = chr$(34) and b = 0 then
                        b = i
                        e = 0
                        p = ""
                    end if
                    i += 1
                loop until i > len(text)
                text = temp
                ' reset workaround
                b = 0 : e = 0 : i = 0 : p = "" : dummy = ""
            end if
            ' remove trailing comma at end of record
            if mid(text, len(text) - 1) = chr$(34) + "," then
                text = mid(text, 1, len(text) - 1)
            end if
            explode(text, ",", ordinance())
            if UBound(ordinance) <> fieldnr then
                logentry("fatal", "error unequal amount of fields at line " & cnt + 1 & " " + replace(text, "|", ""))
            end if
            For x As Integer = 1 To UBound(ordinance)
                ' restore data "... , ..." structure
                ordinance(x) = replace(trim(ordinance(x)), chr$(34), "")
                ordinance(x) = replace(trim(ordinance(x)), "|\|", ",")
                ' check validty data 
                if instr(trim(ordinance(x)), "'") > 0 then
                    logentry("warning", "found unescaped ' modified with '' at line " & cnt + 1 & " " + text)
                end if    
                if x <> UBound(ordinance) then
                    dummy += "'" + replace(trim(ordinance(x)), "'", "''") + "',"
                else
                    dummy += "'" + replace(trim(ordinance(x)), "'", "''") + "'"
                end if
            Next
            Print "insert into '" + tbname + "' values (" + dummy + ");" 
        end if
        cnt += 1
    Loop
    print "commit;"
    close(f)
    logentry("notice", "exported csv " + filename + " to sql with tablename " + tbname + " #recs " & cnt)

    return true
    
end function

' cheap json to sql export
Function json2sql(filename as string, tbname as string = "", tabletype as string = "") As boolean

    Dim f       As long
    dim g       as long
    Dim cnt     As integer = 0
    Dim fieldnr As integer = 0
    'dim recnr   as integer = 0
    dim chk     as boolean = false
    dim dbchk   as boolean = false
    Dim text    As String
    dim dummy   as string = ""
    dim dummy2  as string = ""    

    ' filter out ext
    tbname = left(filename, instrrev(filename, ".") - 1)
    ' filter out preceding path if present
    tbname = lcase(mid(tbname, instrrev(tbname, "\") + 1))

    if FileExists(filename) = false then
        logentry("fatal", "file not found or missing..'" & filename & "'")
    end if

    ' handle one liner json
    f = FreeFile
    Open filename For input As #f
        cnt = 0
        Do Until EOF( f )
            Line Input #f, text
            cnt +=1
        loop
        recnr = cnt
        if cnt = 1 then
            g = freefile
            open exepath + "\temp.json" for output as #g
                text = replace(text, "[", "[" + chr$(13) + chr$(10))
                text = replace(text, "},", "}," + chr$(13) + chr$(10))
                text = replace(text, "]", chr$(13) + chr$(10) + "]" + chr$(13) + chr$(10))
                print #g, text
            close(g)
            logentry ("notice", "converted one line json " + filename)
            filename = exepath + "\temp.json"
        end if
    close(f)

    ' handle pjson aka pretty json
    if cnt > 1 then
        f = FreeFile
        Open filename For input As #f
            cnt   = 0
            recnr = 0
            Do Until EOF( f )
                Line Input #f, text
                cnt +=1
                text = trim(text)
                if right(text, 2) = "}," and len(text) > 2 then
                    ' multi line json but not pjson
                    goto skippjson
                    close(f)
                else
                    ' hack remove spaces after colon : (is allowed according to json rfc ....)
                    ' todo expand beyond 3 spaces after colon
                    text = replace(text, chr$(34) + ": ", chr$(34) + ":" + chr$(34))
                    text = replace(text, chr$(34) + ": ", chr$(34) + ":" + chr$(34))
                    text = replace(text, chr$(34) + ": ", chr$(34) + ":" + chr$(34))
                    select case text
                        case "["
                            dummy = "[" + chr$(13,10)
                        case "{"
                            dummy += "{"
                        case "}"
                            dummy += "}" + chr$(13,10)
                        case "]"
                            dummy += "]"
                        case "},"
                            dummy += "}," + chr$(13,10)
                            recnr += 1
                        case else
                            if right(text, 1) <> "," then
                                text = text + ","
                            end if
                            if right(text, 2) <> chr$(34) + "," then
                                text = mid(text, 1, len(text) - 1) + chr$(34) + ","
                            end if
                            ' flatten nested json array
                            if instr(text, ":") = 0 and right(text, 1) = "," then
                                text = replace(text, chr$(34), "")
                            end if
                            dummy += text
                    end select    
                end if
                ' cleanup records
                dummy = replace(dummy, "[" + chr$(34) + ",", "")
                ' todo fix still leaves an extra comma in flat nested array
                dummy = replace(dummy, "],", chr$(34) + ",")
                dummy = replace(dummy, ":" + chr$(34) + chr$(34), ":" + chr$(34))
                dummy = replace(dummy, "[]", "null")
                dummy2 += dummy
                dummy = ""
            loop
            g = freefile
            open exepath + "\temp.json" for output as #g
                print #g, dummy2
            close(g)
            logentry ("notice", "converted pretty json " + filename)
            logentry ("notice", "exported approximatly " & recnr + 1 & " records") 
            filename = exepath + "\temp.json"
        end if
    close(f)

skippjson:

    print "begin transaction;"
    ' create table defintion
    if tabletype = "fts" then
        print "create virtual table if not exists '" + tbname + "' using fts5("        
    else
        print "create table if not exists '" + tbname + "' ("
    end if
    f = FreeFile
    Open filename For input As #f
    cnt = 0
    Do Until EOF( f )
       Line Input #f, text
' patterns maybe use to set field type numeric
' ":123 > ":"123
' 123} > 123"}
' 123, > 123",
        ' replace null values
        if instr(text, chr$(34) + ":null") > 0 then
            text = replace(text, chr$(34) + ":null", chr$(34) + ":" + chr$(34) + "null" + chr$(34))
            logentry ("warning", "replaced null value" + text)
        end if
        ' hack remove spaces after colon : (is allowed according to json rfc ....)
        ' todo expand beyond 3 spaces after colon
        text = replace(text, chr$(34) + ": ", chr$(34) + ":" + chr$(34))
        text = replace(text, chr$(34) + ": ", chr$(34) + ":" + chr$(34))
        text = replace(text, chr$(34) + ": ", chr$(34) + ":" + chr$(34))
        logentry ("warning", "replaced space after colon" + text)
        if cnt = 1 then
            ReDim As String ordinance(0)
            explode(text, chr$(34) + "," + chr$(34), ordinance())
            For x As Integer = 1 To UBound(ordinance)

' catch last numerical field end record todo evaluate
if instr(ordinance(x), chr$(34) + ":" + chr$(34)) = 0 then
print ordinance(x)
    ordinance(x) = replace(ordinance(x), ":", ":" + chr$(34)) 
    ordinance(x) = replace(ordinance(x), "}", chr$(34) + "}") 
end if

                dummy = lcase(mid(trim(ordinance(x)), 3, instr(trim(ordinance(x)), chr$(34) + ":" + chr$(34)) - 3))
                if tabletype = "fts" then
                    ' rank is a reserved fieldname with fts5
                    if dummy = "rank" then dummy = "ranked" end if
                    if x <> UBound(ordinance) then
                        Print "'" + dummy + "'," 
                    else
                        Print "'" + dummy + "'"
                    end if
                else
                    if x <> UBound(ordinance) then
                        Print "'" + dummy + "'" + space(20 - len(dummy)) + "text" + "," 
                    else
                        Print "'" + dummy + "'" + space(20 - len(dummy)) + "text" 
                    end if
                end if
            Next
            fieldnr = UBound(ordinance)
            print ");"
        end if
        cnt += 1
    loop
    close(f)

    ' create inserts    
    f = FreeFile
    Open filename For input As #f
    cnt     = 0
    Do Until EOF( f )
       Line Input #f, text
        ' replace null values json
        if instr(text, chr$(34) + ":null") > 0 then
            text = replace(text, chr$(34) + ":null", chr$(34) + ":" + chr$(34) + "null" + chr$(34))
            logentry ("warning", "replaced null value" + text)
        end if
        ' hack remove spaces after colon : (is allowed according to json rfc ....)
        ' todo expand beyond 3 spaces after colon
        text = replace(text, chr$(34) + ": ", chr$(34) + ":")
        text = replace(text, chr$(34) + ": ", chr$(34) + ":")
        text = replace(text, chr$(34) + ": ", chr$(34) + ":")
        ' hack for pjson todo evaluate (dramatic speedbump)
        ' old location line 743 / 744 before writing temp file but slows down
        ' parsing quite substanialy
        text = replace(text, chr$(34) + ",}", chr$(34) + "}")

        if cnt > 0 then
            dummy = "'"
            ReDim As String ordinance(0)
            explode(text, chr$(34) + "," + chr$(34), ordinance())
            ' validate data
            if UBound(ordinance) <> fieldnr then
                logentry ("warning", "number of field(s) and value(s) do not match " + text)
            end if
            For x As Integer = 1 To UBound(ordinance)

' catch last numerical field end record todo evaluate
if instr(ordinance(x), chr$(34) + ":" + chr$(34)) = 0 then
    ordinance(x) = replace(ordinance(x), ":", ":" + chr$(34)) 
    ordinance(x) = replace(ordinance(x), "}", chr$(34) + "}") 
end if
                ' unescape double quote if needed
                ordinance(x) = replace(ordinance(x), "\" + chr$(34), chr$(34))
                ordinance(x) = replace(ordinance(x), "'", "''")                        
                dummy += mid(trim(ordinance(x)), instr(trim(ordinance(x)), chr$(34) + ":" + chr$(34)) + 3)
                if x <> UBound(ordinance) then
                        dummy += "','"
                else
                    if instrrev(dummy, "},") > 0 then
                        dummy = mid(dummy, 1, len(dummy) - 3) + "'"
                    else
                        dummy = mid(dummy, 1, len(dummy) - 2) + "'"
                    END IF
                end if
            Next

            IF dummy <> "''" then
                ' handle missing fields (allowed in json) see:
                ' https://noobtomaster.com/jackson/handling-different-json-data-types-null-values-and-missing-fields/
                dummy2 = ""
                if UBound(ordinance) < fieldnr then
                    For x As Integer = UBound(ordinance) to fieldnr - 1
                        dummy2 += ",null"
                    next
                dummy += dummy2
                end if
                ' work around te restore null value
                ' todo find better solution see listjson
                dummy = replace(dummy, "'null'", "null")
                Print "insert into '" + tbname + "' values (" + dummy + ");" 
            end if
        end if
        cnt += 1
    Loop
    print "commit;"

    close(f)
    delfile(exepath + "\temp.json")
    logentry("notice", "exported json " + filename + " to sql with tablename " + tbname + " #recs " & cnt)
    return true
    
end function

' cheap xml to sql export
Function xml2sql(filename as string, tbname as string = "", element as string = "", tabletype as string = "") As boolean

    Dim f       As long
    Dim cnt     As integer = 0
    dim chk     as boolean = false
    dim dbchk   as boolean = false
    dim tbchk   as boolean = false
    Dim text    As String
    dim fname   as string    
    dim fvalue  as string    
    dim dummy   as string = ""
    dim dummy2  as string = ""
    dim dbname  as string = ""
    ' filter out ext
    tbname = left(filename, instrrev(filename, ".") - 1)
    ' filter out preceding path if present
    tbname = lcase(mid(tbname, instrrev(tbname, "\") + 1))

    if FileExists(filename) = false then
        logentry("fatal", "file not found or missing..'" & filename & "'")
    end if

    f = FreeFile
    Open filename For input As #f
    print "begin transaction;"

    Do Until EOF( f )
       Line Input #f, text
        select case true
            ' get node meta, version etc
            case instr(text, "<?") > 0
            ' print "meta   ";text
            ' get node dbname
            case instr(text, "<") > 0 and instr(text, "/") = 0 and dbchk = false
                if instr(text, " ") > 0 then
                    dbname = mid(text, 2, instr(trim(text), " ") - 2)
                else
                    dbname = mid(trim(text), 2, len(trim(text)) - 2)
                end if
                dbchk = true
            ' get node tbname or record
            case instr(text, ">") > 0  and instr(text, "/") = 0 and dbchk
                if instr(trim(text), " ") > 0 then
                    tbname = mid(text, 2, instr(trim(text), " ") - 2)
                else
                    tbname = mid(trim(text), 2, len(trim(text)) - 2)
                end if
                dummy += "insert into '" + tbname + "' values ('"
                cnt += 1
            ' get nodes field and value
            ' todo cleanup loops too much truncation is needed
            case else
                if element = "" then
                    if instr(text, "</" + tbname + ">") = 0 and text <> "</" + dbname + ">" then
                        fname = mid(text, instr(text, "<") + 1, instr(trim(text), ">") - 2)
                        ' create table defintion
                        if tbchk = false then
                            if tabletype = "fts" then
                                ' rank is a reserved fieldname with fts5
                                if fname = "rank" then fname = "ranked" end if
                                if fname <> "" then
                                    dummy2 += "'" + fname + "'," + chr(13) + chr$(10) 
                                else
                                    dummy2 += "'" + fname + "'" + chr(13) + chr$(10)
                                end if
                            else
                                if fname <> "" then
                                    dummy2 += "'" + fname + "'" + space(20 - len(fname)) + "text" + "," + chr(13) + chr$(10) 
                                else
                                    dummy2 += "'" + fname + "'" + space(20 - len(fname)) + "text" + chr(13) + chr$(10) 
                                end if
                            end if
                        end if
                        ' create inserts
                        fvalue = mid(text, instr(text, ">") + 1, instr(trim(text), "</") - (len(fname) + 3))
                        if fvalue = "" then fvalue = "null" end if
                        ' reverse xml sanitazion
                        fvalue = replace(fvalue, " &amp; ", " & ")
                        fvalue = replace(fvalue, "&gt;", ">")
                        fvalue = replace(fvalue, "&lt;", "<")
                        fvalue = replace(fvalue, "'", "''")
                        dummy += fvalue + "','"
                    else
                        ' work around te restore null value
                        ' todo find better solution see listjson
                        dummy = replace(dummy, "'null'", "null")
                        dummy = mid(dummy, 1, len(dummy) - 2) + ");" + chr(13) + chr$(10)
                        tbchk = true
                    end if
                end if
        end select
    Loop
    close(f)
    if tabletype = "fts" then
        dummy2 = "create virtual table if not exists '" + tbname + "' using fts5(" + chr#(13) + chr$(10) + dummy2
    else
        dummy2 = "create table if not exists '" + tbname + "' (" + chr#(13) + chr$(10) + dummy2
    end if
    print mid(dummy2, 1, len(dummy2) - 3) + chr(13) + chr$(10) + ");"
    print mid(dummy, 1, len(dummy) - 4)
    print "commit;"

    if element <> "" then
        if chk = false then
            logentry("warning", filename + " searching for '" + element + "' element not found")
        else
            logentry("notice", filename + " searching for '" + element + "' element found " & cnt & " recs")
        end if
    else
        logentry("notice", filename + " found " & cnt & " recs")
    end if
    logentry("notice", "exported xml " + filename + " to sql with tablename " + tbname + " #recs " & cnt)
    return true
    
end function

' export folder filespec to supported file types
' current csv, json, html, sql and xml
' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function dir2file(folder as string, filterext as string, listtype as string = "sql", htmloutput as string = "default") as integer
    ' setup filelist
    dim                as integer i = 1, j=1, n = 1, attrib, itemnr, maxfiles
    dim                as long tmp, f
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
    f = freefile

    #ifdef __FB_LINUX__
      const pathchar = "/"
    #else
      const pathchar = "\"
    #endif

    select case listtype
        case "html"
            ' get template for body, css, and javacript    
            tmp = readfromfile(exepath  + "\templates\head.html")
            Do Until EOF(tmp)
                Line Input #tmp, dummy
                print dummy    
                itemnr += 1
            Loop
            close(tmp)
            dummy = ""
            if instr(filterext, ".mp3") > 0 and htmloutput = "exif" then
                ' table header todo needs to be refactored and compressed
                print "<table class='sortable' id='datatable'>"
                print "  <thead><tr>"
                print "   <th width=20px;>"
                print "     <div class='trdropdown'><button class='trdropbtn'></button><div class='trdropdown-content'>"
                print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '1')" + chr$(34) + ";>artist</a>"
                print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '2')" + chr$(34) + ";>title</a>"
                print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '3')" + chr$(34) + ";>album</a>"
                print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '4')" + chr$(34) + ";>genre</a>"
                print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '5')" + chr$(34) + ";>theme</a>"
                print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '6')" + chr$(34) + ";>year</a>"
                print "     </div></div>"
                print "   </th>"
                print "   <th>artist</th>"
                print "   <th>title</th>"
                print "   <th>album</th>"
                print "   <th>genre</th>"
                print "   <th>theme</th>"
                print "   <th>year</th>"
                print "  </tr></thead>"
            else
                if (instr(filterext, ".jpg") > 0 or instr(filterext, ".png") > 0) and htmloutput = "exif" then
                    ' table header
                    print "<table class='sortable' id='datatable'>"
                    print "  <thead><tr>"
                    print "   <th width=20px;>"
                    print "     <div class='trdropdown'><button class='trdropbtn'></button><div class='trdropdown-content'>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '1')" + chr$(34) + ";>filename</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '2')" + chr$(34) + ";>coverwidth</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '3')" + chr$(34) + ";>coverheight</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '4')" + chr$(34) + ";>orientation</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '5')" + chr$(34) + ";>filesize</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '6')" + chr$(34) + ";>thumbnail</a>"
                    print "     </div></div>"
                    print "   </th>"
                    print "   <th>filename</th>"
                    print "   <th>coverwidth</th>"
                    print "   <th>coverheight</th>"
                    print "   <th>orientation</th>"
                    print "   <th>filesize</th>"
                    print "   <th>thumbnail</th>"
                    print "  </tr></thead>"
                else
                    ' table header todo drop down filter needs to alter javascript tdelement ui name
                    print "<table class='sortable' id='datatable'>"
                    print "  <thead><tr>"
                    print "   <th width=20px;>"
                    print "     <div class='trdropdown'><button class='trdropbtn'></button><div class='trdropdown-content'>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '1')" + chr$(34) + ";>path</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '2')" + chr$(34) + ";>name</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '3')" + chr$(34) + ";>ext</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '4')" + chr$(34) + ";>size</a>"
                    print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '5')" + chr$(34) + ";>date</a>"
                    print "     </div></div>"
                    print "   </th>"
                    print "   <th>path</th>"
                    print "   <th>name</th>"
                    print "   <th>ext</th>"
                    print "   <th>size</th>"
                    print "   <th>date</th>"
                    print "   <th>attr</th>"
                    print "  </tr></thead>"
                end if
            end if
    end select

    ' read dir recursive starting directory
    path(1) = folder 
    if( right(path(1), 1) <> pathchar) then
        file = dir(path(1), fbNormal or fbDirectory, @attrib)
        if( attrib and fbDirectory ) then
            path(1) += pathchar
        end if
    end if

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
                        case "csv", "xml", "json", "sql"
                            if instr(filterext, ".mp3") > 0 and htmloutput = "exif" then
                                ' path(i) folder and drive
                                getmp3baseinfo(path(i) + file)
                                argc(0) = "artist"
                                argc(1) = "title"
                                argc(2) = "album"
                                argc(3) = "year"
                                argc(4) = "genre"
                                argc(5) = "theme"

                                argv(0) = taginfo(1)
                                argv(1) = taginfo(2)
                                argv(2) = taginfo(3)
                                argv(3) = taginfo(4)
                                argv(4) = taginfo(5)
                                argv(5) = taginfo(6)
                            else
                                if (instr(filterext, ".jpg") > 0 or instr(filterext, ".png") > 0) and htmloutput = "exif" then
                                    getimagemetric(path(i) + file)
                                    argc(0) = "filename"
                                    argc(1) = "coverwidth"
                                    argc(2) = "coverheight"
                                    argc(3) = "orientation"
                                    argc(4) = "filesize"
                                    argc(5) = "thumbnail"

                                    argv(0) = path(i) + file
                                    argv(1) = str(coverwidth)
                                    argv(2) = str(coverheight)
                                    argv(3) = orientation
                                    argv(4) = str(fsize)
                                    argv(5) = str(thumb)
                                else
                                    argc(0) = "path"
                                    argc(1) = "file"
                                    argc(2) = "fileext"
                                    argc(3) = "fsize"
                                    argc(4) = "fdate"
                                    argc(5) = "fattr"

                                    argv(0) = path(i)
                                    argv(1) = file
                                    argv(2) = fileext
                                    argv(3) = str(fsize)
                                    argv(4) = fdate
                                    argv(5) = fattr
                                end if
                            end if

                            ' get text in file if needed
                            if instr(filterext, ".txt") > 0 and htmloutput = "exif" then
                                argc(5) = "content"
                                dummy   = ""
                                argv(5) = ""
                                tmp = readfromfile(path(i) + file)
                                Do Until EOF(tmp)
                                    Line Input #tmp, dummy
                                    argv(5) += dummy + chr$(13) + chr$(10)
                                Loop
                                close(tmp)
                            end if

                            ' create index from text files import
                            if instr(filterext, ".txt") > 0 and htmloutput = "index" then
                                argc(5) = "dictionary"
                                'dim wc as wordtally
                                argv(5) = dictonary(path(i) + file, wc)
                            end if

                            For j As Integer = 0 To 5
                                redim preserve record.fieldname(0 to recnr + 5)
                                redim preserve record.fieldvalue(0 to recnr + 5)
                                record.fieldname(recnr)  = argc(j)
                                record.fieldvalue(recnr) = argv(j)
                                recnr += 1
                            Next j
                        case "html"
                            ' create html5 audioplayer
                            if instr(filterext, ".mp3") > 0  and htmloutput = "exif" then
                                ' path(i) folder and drive
                                getmp3baseinfo(path(i) + file)
                                print "<tr class='trlight' onclick=" + chr$(34) + "audioplay('file://" + replace(path(i), "\", "/") + file + "', this);" + chr$(34) + ">" + _
                                          "<td><div class='audiobutton'></div></td>" _
                                          + "<td>" + taginfo(1) + "</td>" _
                                          + "<td>" + taginfo(2) + "</td>" _
                                          + "<td>" + taginfo(3) + "</td>" _
                                          + "<td>" + taginfo(5) + "</td>" _
                                          + "<td>" + taginfo(6) + "</td>" _
                                          + "<td>" + taginfo(4) + "</td>" _
                                          + "</tr>"
                            '                    print ".. adding " + taginfo(1) + " - " +  taginfo(2)
                            else
                                if (instr(filterext, ".jpg") > 0 or instr(filterext, ".png") > 0) and htmloutput = "exif" then
                                    getimagemetric(path(i) + file)
                                    print "     <tr class='trlight' onclick=" + chr$(34) + "document.getElementById('myModal').style.display='block'; currentDiv(" _ 
                                                              & maxfiles + 1 & ");" + chr$(34) + ">"
                                    print "        <td><img class=" + chr$(34) + "tdthumb" + chr$(34) + " src=" + chr$(34) _
                                                    + "file://" + replace(path(i), "\", "/") + file + chr$(34) + "></td>"
                                    print "        <td>" + path(i) + file + "</td>"
                                    print "        <td>" + str(coverwidth) + "</td>"
                                    print "        <td>" + str(coverheight) + "</td>"
                                    print "        <td>" + orientation + "</td>"
                                    print "        <td>" + str(fsize) + "</td>"
                                    print "        <td>" + str(thumb) + "</td>"
                                    print "     </tr>"
                                else
                                    print "     <tr class='trlight'>"
                                    print "        <td></td>"
                                    print "        <td>" + path(i) + "</td>"
                                    print "        <td>" + file + "</td>"
                                    print "        <td>" + fileext + "</td>"
                                    print "        <td>" & fsize & "</td>"
                                    print "        <td>" + fdate + "</td>"
                                    print "        <td>" + fattr + "</td>"
                                    print "     </tr>"
                                end if
                            end if
                            ' create imageviewer A
                            if instr(filterext, ".jpg") > 0  and htmloutput = "exif" then
                                dummy += "        <div class=" + chr$(34) + "w3-display-container mySlides" + chr$(34) _
                                                            + ">" + chr$(13) + chr$(10)
                                dummy += "          <img class=" + chr$(34) + "w3-animate-left ovimage" + chr$(34) + " src=" + chr$(34) _
                                                                 + "file://" + replace(path(i), "\", "/") + file + chr$(34) + ">" + chr$(13) + chr$(10)
                                dummy += "          <div class=" + chr$(34) + "w3-display-bottomleft-stretch w3-container w3-padding-8 w3-black" _
                                                                 + chr$(34) + ">" + chr$(13) + chr$(10)
                                dummy += "          " + replace(replace(file, "_", " "), ".jpg", "") + chr$(13) + chr$(10)
                                dummy += "          </div>" + chr$(13) + chr$(10)
                                dummy += "        </div>" + chr$(13) + chr$(10)

                                dummy2 += "        <img class=" + chr$(34) + "demo w3-opacity w3-hover-opacity-off ovthumb" + chr$(34) _
                                                               + " src=" + chr$(34) + "file://" + replace(path(i), "\", "/") + file + chr$(34) _
                                                               + " onclick=" + chr$(34) + "currentDiv(" & maxfiles + 1 & ")" + chr$(34) + ">" + chr$(13) + chr$(10)
                            end if
                    end select
                    maxfiles += 1
                else
                    'logentry("warning", "file format not supported - " + path(i) & file)
                end if    
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend

    select case listtype
        case "html"
            ' table footer
            print "</table>"

            ' create imageviewer B
            if instr(filterext, ".jpg") > 0 then
                print "</div>"
                print "<!-- overlay for image navigation -->
                print "<div id=" + chr$(34) + "myModal" + chr$(34) + " class=" + chr$(34) + "modal" + chr$(34) + ">"
                print "  <span class='playslide'>"
                print "         <a href='templates/slide.html' style='text-decoration: none;' target='_blank'>"
                print "            <svg class='svglight' viewBox='0 0 32 3' height='16px' width='16px' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>"
                print "            <path d='M1,14c0,0.547,0.461,1,1,1c0.336,0,0.672-0.227,1-0.375L14.258,9C14.531,8.867,15,8.594,15,8s-0.469-0.867-0.742-1L3,1.375  C2.672,1.227,2.336,1,2,1C1.461,1,1,1.453,1,2V14z'/>"
                print "            </svg>"
                print "        </a>&nbsp;&nbsp;"
                print "  </span>"
                print "  <span class=" + chr$(34) + "close" + chr$(34) + ">&times;</span>"
                print "   <p id=" + chr$(34) + "time" + chr$(34) + "></p>"
                print "   <p id=" + chr$(34) + "date" + chr$(34) + "></p>"
                print "    <div class=" + chr$(34) + "w3-content w3-display-container" + chr$(34) + ">"

                print dummy

                print "        <!-- image navigation left and right -->"
                print "        <div class=" + chr$(34) + "w3-text-white w3-display-middle" + chr$(34) _
                                            + " style=" + chr$(34) + "top:350px;width:90%" + chr$(34) + ">"
                print "             <div class=" + chr$(34) + "w3-left w3-hover-text-khaki" + chr$(34) _
                                                 + "  onclick=" + chr$(34) + "plusDivs(-1)" + chr$(34) + ">&#10094;</div>"
                print "             <div class=" + chr$(34) + "w3-right w3-hover-text-khaki" + chr$(34) _
                                                 + " onclick=" + chr$(34) + "plusDivs(1)" + chr$(34) + ">&#10095;</div>"
                print "        </div>"
                print "        <!-- <div class=" + chr$(34) + "w3-row-padding w3-section" + chr$(34) + "> -->"
                print "        <div class=" + chr$(34) + "ovthumbbox" + chr$(34) + ">"

                print mid(dummy2, 1, len(dummy2) - 2)
                print "        </div>"
                print "    </div>"

                dummy  = ""
                dummy2 = ""
            end if

            ' get template for footer, css, and javacript    
            tmp = readfromfile(exepath  + "\templates\footer.html")
            Do Until EOF(tmp)
                Line Input #tmp, dummy
                print dummy    
                itemnr += 1
            Loop
            close(tmp)
            close(f)
    end select

    ' todo find out why last record is empty
    recnr = recnr - 1
    logentry("notice", "found in " + folder + " with filespec " + filterext + " " & maxfiles & " files")
    return maxfiles

end function

' cheap ini file searcher
Function readinikeyvalue( filename as string, section as string, inikey as string ) as boolean

    if FileExists(filename) = false then
        logentry("error", "reading " + filename + " file does not excist")
    end if    

    Dim f As long
    Dim text As String

    f = FreeFile
    Open filename For input As #f
    logentry("notice", filename + " searching" + " with section " + section + " for key " + inikey)

    Do Until EOF(f)
        Line Input #f, text
        ' check if section is found in the current line
        If LCase( text ) = "[" & LCase( section ) & "]" Then
            ' parse lines until the next section is reached
            Do until eof(f)
                Line Input #f, text
                if instr(text, inikey + "=") > 0 then
                    if mid(text, instr(text, "=") + 1, 1) = "" then
                        logentry("warning", filename + " searching" + " with section " + section + " with key " + inikey + " key value is blank")
                    else                      
                        print text
                    end if    
                end if
                if Left( text, 1 ) = "[" then
                    exit do
                end if    
            'logentry("warning", filename + " searching" + " with section " + section + "key not found")
            Loop
        end if
    Loop
    close(f)
    'logentry("notice", filename + " searching" + " with section " + section + " not found")
    return true
End Function

' cheap ini file reader
Function readini(filename as string) as boolean
    dim itm    as string
    dim inikey as string
    dim inival as string
    dim f      as long
    f = readfromfile(filename)
    Do Until EOF(f)
        Line Input #f, itm
        if instr(1, itm, "=") > 1 then
            inikey = trim(mid(itm, 1, instr(1, itm, "=") - 2))
            inival = trim(mid(itm, instr(1, itm, "=") + 2, len(itm)))
            'print inikey + " - " + inival
        end if    
    loop    
    close(f)
return true
end function

' text related functions
' ______________________________________________________________________________'

' split or explode by delimiter return elements in array
' based on https://www.freebasic.net/forum/viewtopic.php?t=31691 code by grindstone
Function explode(haystack As String = "", delimiter as string, ordinance() As String) As UInteger
    Dim As UInteger b = 1, e = 1   'pointer to text, begin and end
    Dim As UInteger x              'counter

    Do Until e = 0
      x += 1
      ReDim Preserve ordinance(x)             'create new array element
      e = InStr(e + 1, haystack, delimiter)   'set end pointer to next space
      ordinance(x) = Mid(haystack, b, e - b)  'cut text between the pointers and write it to the array
      b = e + 1                               'set begin pointer behind end pointer for the next word
    Loop

    Return x 'nr of elements returned

    ' sample code for calling the function explode
    'ReDim As String ordinance(0)
    'explode("The big brown fox jumped over; the lazy; dog", ";", ordinance())
    'print UBound(ordinance)
    'For x As Integer = 1 To UBound(ordinance)
    '    Print ordinance(x)
    'Next

End Function

' setup wrap string
type stringwrap
    as integer  linecnt                ' current line
    as integer  linemax                ' max viewable lines
    as integer  linelength             ' max line length
    as integer  wrapcharpos            ' position to wrap on with wrapchar
    as string   wrapchar               ' wrap character , . etc
    as string   lineitem               ' line content
    as string   linetemp               ' temp line when wraping
    as boolean  filternonalphanumeric  ' filter lines out with no alphanumeric characters
end type

dim swp as stringwrap
swp.linecnt               = 1
swp.linemax               = 10
' todo needs to be proportional to background and font size text box
swp.linelength            = 62
swp.wrapchar              = " ,.?;-"
swp.filternonalphanumeric = true

function replace(byref haystack as string, byref needle as string, byref substitute as string) as string
'found at https://freebasic.net/forum/viewtopic.php?f=2&t=9971&p=86259&hilit=replace+character+in+string#p86259
    dim as string temphaystack = haystack
    dim as integer fndlen = len(needle), replen = len(substitute)
    dim as integer i = instr(temphaystack, needle)

    while i
        temphaystack = left(temphaystack, i - 1) & substitute & mid(temphaystack, i + fndlen)
        i = instr(i + replen, temphaystack, needle)
    wend

    return temphaystack

end function

' check if string contains alphanumeric value
' courtesy counting_pine https://www.freebasic.net/forum/viewtopic.php?p=166250&hilit=isalphanum#p166250 
function isalphanumeric(haystack as string) as boolean
    dim i as integer
    do
        select case asc(mid(haystack, i, 1))
            case asc("0") to asc("9"), asc("A") to asc("Z"), asc("a") to asc("z")
            return true
        end select
        i += 1
    loop until i > len(haystack)

    return false

end function

function htmlcleanup(haystack as string) as string
    ' generic replace for text and html
    haystack = Replace(haystack, "  ", "")
    haystack = Replace(haystack, "=A0", " ")
    haystack = Replace(haystack, "=A9", "©")
    haystack = Replace(haystack, "=20", " ")
    haystack = Replace(haystack, "=3D", "=")
    haystack = Replace(haystack, "=09", " ")
    haystack = Replace(haystack, "=C2", " ")
    haystack = Replace(haystack, "=F6", "")
    haystack = Replace(haystack, "=92", "'")
    haystack = Replace(haystack, "=93", "'")
    haystack = Replace(haystack, "=94", "'")
    haystack = Replace(haystack, "=95", "-")
    haystack = Replace(haystack, "=96", "")
    haystack = Replace(haystack, "=E2=80=93", "-")
    haystack = replace(haystack, "=E2=80=99", "")
    haystack = replace(haystack, chr$(9), "")

    return haystack

end function


' found at https://www.freevbcode.com/ShowCode.asp?ID=1037
function striphtmltags(html as string) as string

    dim bpos as integer = InStr(html, "<")
    dim epos as integer = InStr(html, ">")
    dim dummy as string
    
    Do While bpos <> 0 And epos <> 0 And epos > bpos
          dummy = Mid(html, bpos, epos - bpos + 1)
          html = replace(html, dummy, "")
          bpos = InStr(html, "<")
          epos = InStr(html, ">")
    Loop

    ' Translate common escape sequence chars
    html = Replace(html, "&nbsp;", " ")
    html = Replace(html, "&amp;", "&")
    html = Replace(html, "&quot;", "'")
    html = Replace(html, "&#", "#")
    html = Replace(html, "&lt;", "<")
    html = Replace(html, "&gt;", ">")
    html = Replace(html, "%20", " ")
    html = LTrim(Trim(html))

    return html

end function

function wordwrap2file(filename as string, swp as stringwrap) as boolean

    dim dummy   as string  = ""
    dim orgname as string  = ""
    dim tempfolder as string  = ""
    dim temp    as string  = ""
    dim buffer  as string  = ""
    dim linecnt as integer = 0
    dim j       as integer = 0
    dim i       as integer = 1
    dim f       as long
    f = freefile

    orgname = mid(filename, instrrev(filename, "\") + 1)
    orgname = left(orgname, len(orgname) - 4) + ".txt"

    ' filter html todo messy needs to be cleaned up
    if instr(filename, ".mht") > 0 then
        tempfolder = mid(filename, instrrev(filename, "\"))
        tempfolder = exepath + mid(tempfolder, 1, instrrev(tempfolder, ".") - 1)
        open filename for input as #f
            do until eof(f)
                line input #f, swp.lineitem
                    ' remove frontpage thing sticks = to end of line
                    if mid(swp.lineitem, len(swp.lineitem)) = "=" then
                        swp.lineitem = mid(swp.lineitem, 1, len(swp.lineitem) - 1)
                    end if
                temp = temp + swp.lineitem
            loop
        close(f)

        if instr(lcase(temp), "<body>") > 0 then
            temp = mid(temp, instr(lcase(temp), "<body>"), instr(lcase(temp), "</body>") - instr(lcase(temp), "<body>") )
        end if
        if instr(lcase(temp), "<body ") > 0 then
            temp = mid(temp, instr(lcase(temp), "<body "), instr(lcase(temp), "</body>") - instr(lcase(temp), "<body ") )
        end if

        temp = replace(temp, "</p>", "||")
        temp = replace(temp, "</P>", "||")
        temp = replace(temp, "<br>", "||")
        temp = replace(temp, "<BR>", "||")
        temp = replace(temp, "</span>", "||")
        temp = replace(temp, "</SPAN>", "||")
        temp = htmlcleanup(temp)
        temp = striphtmltags(temp)
        temp = replace(temp, "||", chr$(13) + chr$(10))

        filename = exepath + "\html.tmp"
        f = freefile
        open filename for output as #f
            print #f, trim(temp)
        CLOSE(f)
    end if

    ' detect unicode utf16 endian big / little
    f = freefile
    Open filename For Binary Access Read As #f
        If LOF(f) > 0 Then
            buffer = String(LOF(f), 0)
            Get #f, , buffer
        End If
    Close(f)
    if instr(1, mid(buffer,1, 1), chr$(255)) > 0 and instr(1,  mid(buffer,2, 1), chr$(254)) > 0_
       or instr(1, mid(buffer,1, 1), chr$(254)) > 0 and instr(1,  mid(buffer,2, 1), chr$(255)) then
        open filename for input encoding "utf16" as #f
    else
        open filename for input as #f
    end if

    open tempfolder + "\" + orgname for output as #20
    do until eof(f)
        line input #f, swp.lineitem
        j = 0
        swp.linetemp = ""

        if swp.filternonalphanumeric then
            if isalphanumeric(swp.lineitem) = false then
                swp.lineitem = ""
                'print #20, ""
                'goto skipprint:
            end if
        end if

        'cleanup string tab, etc
        'swp.lineitem = replace(swp.lineitem, "   ", " ")
        'swp.lineitem = replace(swp.lineitem, "  ", " ")
        swp.lineitem = replace(swp.lineitem, chr$(9), "  ")

        ' ghetto latin-1 support
        swp.lineitem = replace(swp.lineitem, chr$(130), ",")
        swp.lineitem = replace(swp.lineitem, chr$(132), chr$(34))
        swp.lineitem = replace(swp.lineitem, chr$(139), "<")
        swp.lineitem = replace(swp.lineitem, chr$(145), "'")
        swp.lineitem = replace(swp.lineitem, chr$(146), "'")
        swp.lineitem = replace(swp.lineitem, chr$(147), chr$(34))
        swp.lineitem = replace(swp.lineitem, chr$(148), chr$(34))
        swp.lineitem = replace(swp.lineitem, chr$(150), "-")
        swp.lineitem = replace(swp.lineitem, chr$(152), "~")
        swp.lineitem = replace(swp.lineitem, "•", "-")
        swp.lineitem = replace(swp.lineitem, "~", "-")

        ' special case no space in line or multiple returns
        if instr(swp.lineitem, " ") = 0 then
            if len(swp.lineitem) > swp.linelength then
                swp.lineitem = mid(swp.lineitem, 1, swp.linelength - 2)
            end if
            if len(swp.lineitem) = 0 then
                linecnt += 1
            end if
            if linecnt > 1 then
                linecnt = 0
                goto skipprint:
            end if
        end if

        if len(swp.lineitem) > swp.linelength and instrrev(swp.lineitem, " ") > swp.linelength * 1.2 then
            do while j <= fix(len(swp.lineitem) / swp.linelength)
                i = 1
                dummy = mid(swp.lineitem, j * swp.linelength + 1, swp.linelength)
                ' move wrappos to pos wrapchar instead of linelength if possible
                do while i <= len(swp.wrapchar)
                    swp.wrapcharpos = instrrev (mid(dummy, 1, swp.linelength), mid(swp.wrapchar, i, 1))
                    if  swp.linelength <= swp.wrapcharpos + len(mid(dummy, swp.wrapcharpos, len(dummy))) then
                        exit do
                    end if
                    i += 1
                loop

                ' special case no wrapchar
                if swp.wrapcharpos > 0 then
                    swp.linetemp = swp.linetemp + mid(dummy, 1, swp.wrapcharpos) + chr$(13) + chr$(10)_
                                    + trim(mid(dummy, swp.wrapcharpos, len(dummy)))
                else
                    ' note just chr$(13) truncates linetemp todo evaluate without chr$(10)
                    'swp.linetemp = swp.linetemp + dummy + chr$(13) + chr$(10)
                    swp.linetemp = swp.linetemp + dummy' + chr$(13)
                end if
                j += 1
                ' brute force paragraphs
                'if swp.linecnt > swp.linemax then
                '    swp.linetemp = swp.linetemp + chr$(13) + chr$(10) + chr$(13) + chr$(10)
                '    swp.linecnt = 1
                'end if        
                swp.linecnt += 1
            loop
            swp.lineitem = swp.linetemp
        end if

        print #20, swp.lineitem
skipprint:    
    loop
    close #20
    close(f)
    delfile(exepath + "\html.tmp")

    return true

end function

' notes check https://www.baeldung.com/cs/ml-similarities-in-text
' https://www.perplexity.ai/search/cfc218e5-beb2-4c7b-82ef-1e44e22572a8?s=u

' get highest value in array
' via https://www.freebasic.net/forum/viewtopic.php?t=25443
' and https://www.perplexity.ai/search/d662160a-25a7-472c-a894-64abaddc742a?s=u 
function arrayhighestvalue(needle as string, wc as wordtally) as integer
    Dim As Integer occurancemax = wc.count(0)
    dim as integer temp, cnt
    For i As Integer = 1 To wclinenr
        If wc.count(i) > occurancemax and wc.word(i) = needle Then
            occurancemax = wc.count(i)
            temp = i
        End If
    Next
    return occurancemax
end function

function arraylongestvalue(arr() as string) as string
    dim longeststr as string = arr(0)
    for i as integer = 1 to ubound(arr)
        if len(arr(i)) > len(longeststr) then
            longeststr = arr(i)
        end if
    next
    return longeststr
end function


function dictonary(filename as string, wc as wordtally) as string
    dim dummy   as string = ""
    dim text    as string = ""
    dim fieldnr as integer = 0
    dim         as long tmp, f
    dim commonwords as string = "a, an, and, any, all, at, be, both, but, by, can, came, come, comes, did, do, does, doing, done, else, end, even, " + _
                                "for, from, go, goes, gone, going, got, had, has, have, having, here, how, i, if, in, into, let, like, " + _ 
                                "made, make, more, most, no, not, now, of, off, on, once, sure, that, the, their, them, then, they, there, these, thing, " + _ 
                                "this, to, too, use, used, using, " + _
                                "want, was, way, we, well, with, what, when, where, who, would, yes, you, your"
    wclinenr = 0
    ' isolate words
    tmp = readfromfile(filename)
    Do Until EOF(tmp)
        Line Input #tmp, dummy
        if len(dummy) > 2 then 
            'print len(dummy)    
            ReDim As String ordinance(0)
            explode(dummy, " ", ordinance())
            For x As Integer = 1 To UBound(ordinance)
                ordinance(x) = trim(lcase(ordinance(x)))
                if ordinance(x) <> "" and instr(ordinance(x), " ") = 0 and (len(ordinance(x)) > 4 and len(ordinance(x)) < 21) then
                    ' todo capture word...word patterns
                    ordinance(x) = replace(ordinance(x), ".", "")
                    ordinance(x) = replace(ordinance(x), "!", "")
                    ordinance(x) = replace(ordinance(x), "?", "")
                    ordinance(x) = replace(ordinance(x), ",", "")
                    ordinance(x) = replace(ordinance(x), "'", "")
                    ordinance(x) = replace(ordinance(x), ":", "")
                    ordinance(x) = replace(ordinance(x), ";", "")
                    ordinance(x) = replace(ordinance(x), ")", "")
                    ordinance(x) = replace(ordinance(x), "(", "")
                    ordinance(x) = replace(ordinance(x), "*", "")
                    ordinance(x) = replace(ordinance(x), "[", "")
                    ordinance(x) = replace(ordinance(x), "]", "")
                    ordinance(x) = replace(ordinance(x), chr$(34), "")
                    'print recnr & " " & ordinance(x)
                    wclinenr += 1
                end if
                redim preserve wc.word(0 to wclinenr)
                redim preserve wc.count(0 to wclinenr)
                wc.word(wclinenr)  = ordinance(x)
                ' tally word occurance
                for j as integer = 1 to wclinenr
                    'with record
                        if wc.word(wclinenr) = wc.word(j) then
                            wc.count(wclinenr) += 1
                        end if
                    'end with
                next j
            next
        end if
    Loop
    close(tmp)

    ' filter on min / max frequncy word
    for j as integer = 1 to wclinenr 
        'with record
            if wc.word(j) <> "" and instr(commonwords, wc.word(j)) = 0 and isalphanumeric(wc.word(j)) and val(wc.word(j)) = 0 then
                if wc.count(j) <= 2 then
                    if wc.count(j) = arrayhighestvalue(wc.word(j), wc) then
                        'print wc.count(j) & " = " + wc.word(j)
                        text += wc.word(j) + ", "
                    end if                    
                end if
            end if
        'end with
    next j
    'print "word count: " & recnr
    return left(text, len(text) - 2) 
end function
