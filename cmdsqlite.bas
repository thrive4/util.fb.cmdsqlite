' import export various file formats csv, json, html, sql and xml
' to and from a sqlite database by thrive4 sept 2023
' more info see: https://github.com/thrive4/util.fb.cmdsqlite

declare function listrecords(needle as string = "") as boolean
declare function  getimagemetric(filename As String) As boolean
' init imagemetric
common shared coverwidth    as integer
common shared coverheight   as integer
common shared orientation   as string
common shared report        as string
common shared thumbnail     as string
common shared thumb         as integer
common shared layout        as string
common shared chunk         as string
common shared csv           as string

#include once "sqlite3.bi"
#include once "windows.bi"
#include once "utilfile.bas"
#include once "utilaudio.bas"
#include once "utilmd5.bas"
#include once "utilmedia.bas"
#include once "utilmht.bas"
#cmdline "app.rc"


' init sqlite
Dim db      As sqlite3 Ptr
Dim rc      As Integer
Dim fn      As String
Dim sel     As String
Dim zErrMsg As Zstring Ptr
recnr = 0

Function sqlitegetrecord Cdecl (Byval usr As Any Ptr, _
                                Byval argc As Integer, _
                                Byval argv As Zstring Ptr Ptr, _
                                Byval colname As Zstring Ptr Ptr) As Integer
    For i As Integer = 0 To argc - 1

        redim preserve record.fieldname(0 to recnr + argc)
        redim preserve record.fieldvalue(0 to recnr + argc)
        If argv[i] <> 0 Then
            'Print *argv[i]
            record.fieldname(recnr)  = *colname[i]
            record.fieldvalue(recnr) = *argv[i]
        else 
            record.fieldname(recnr) = *colname[i]
            record.fieldvalue(recnr) = "null"
        End If
        recnr += 1
    Next i

    Return 0
End Function

' display specific field name value
function getrecord(needle as string) as string
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
        return needle + " not found or empty "
    else
        'print wstr(fieldvalue + space(offset - Len(fieldvalue)) + suffix)
        return record.fieldvalue(1)
    end if
end function

function listcsv(needle as string = "", checkfile as boolean = false) as boolean

    dim dummy   as string = ""
    dim fieldnr as integer = 0
    dim cnt     as integer = 1

    ' get fieldnames aka header
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                if record.fieldname(i) = needle and checkfile then
                    dummy += "coverfound,"
                end if
                dummy += record.fieldname(i) + ","
                fieldnr += 1
            end if
        end with
    next i
    print mid(dummy, 1, len(dummy) - 1)

    ' get fieldvalues aka data
    dummy = ""
    for i as integer = 0 to recnr
        with record
            ' add " delimiter comma is used
            if instr(record.fieldvalue(i), ",") then
                record.fieldvalue(i) = chr$(34) + record.fieldvalue(i) + chr$(34)
            end if
            if record.fieldname(i) = needle and checkfile then
                if FileExists(record.fieldvalue(i)) <> 0 then 
                    dummy += "true,"
                else
                    dummy += "false," 
                end if 
            end if
            if cnt = fieldnr then
                dummy += record.fieldvalue(i) + chr$(13) + chr$(10)
                cnt = 1
            else
                dummy += record.fieldvalue(i) + ","
                cnt += 1
            end if
        end with
    next i
    ' strip final carrige return
    print mid(dummy, 1, len(dummy) - 2)
    ' todo needs more accurate count for fields and records current workaround
    logentry("notice", "csv export nr record(s) " & ((recnr + 1) / fieldnr))

    return true

end function

function listsql(needle as string = "", tabletype as string = "", bom as string = "") as boolean

    dim dummy   as string = ""
    dim fieldnr as integer = 0
    dim cnt     as integer = 1

    ' create table defintion
    print "begin transaction;"
    ' add encoding if needed
    select case bom
        case "utf16", "utf-16"
            print "pragma encoding = utf16;"
        case "utf8", "utf-8"
            print "pragma encoding = utf8;"
        case else
            ' nop
    end select

    if tabletype = "fts" then
        print "create virtual table if not exists '" + needle + "' using fts5("        
    else
    print "create table if not exists '" + needle + "' ("
    end if

    ' get fieldnames aka header
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                if tabletype = "fts" then
                    ' rank is a reserved fieldname with fts5
                    if lcase(trim(record.fieldname(i))) = "rank" then record.fieldname(i) = "ranked" end if
                    dummy += "'" + record.fieldname(i) + "'," + chr$(13) + chr$(10)
                else
                    dummy += "'" + record.fieldname(i) + "'" + space(20 - len(record.fieldname(i))) + "text," + chr$(13) + chr$(10)
                end if
                fieldnr += 1
            end if
        end with
    next i
    print mid(dummy, 1, len(dummy) - 3)
    print ");"

    ' create inserts
    dummy = ""
    for i as integer = 0 to recnr
        with record
            record.fieldvalue(i) = replace(record.fieldvalue(i), "'", "''")
            if cnt = fieldnr then
                dummy += "'" + record.fieldvalue(i) + "');" + chr$(13) + chr$(10)
                cnt = 1
            else
                if cnt = 1 then
                    dummy += "insert into '" + needle + "' values ("
                end if    
                dummy += "'" + record.fieldvalue(i) + "',"
                cnt += 1
            end if
        end with
    next i
    ' strip final carrige return
    print mid(dummy, 1, len(dummy) - 2)
    print "commit;"
    ' todo needs more accurate count for fields and records current workaround
    logentry("notice", "sql export nr record(s) " & ((recnr + 1) / fieldnr))

    return true

end function

function listhtml(needle as string = "") as boolean

    dim dummy   as string = ""
    dim tbname  as string = "game"
    dim fieldnr as integer = 0
    dim cnt     as integer = 0
    dim         as integer itemnr = 1, i = 1, n = 1
    dim         as long tmp, f

    ' get template for body, css, and javacript    
    tmp = readfromfile(exepath  + "\templates\head.html")
    Do Until EOF(tmp)
        Line Input #tmp, dummy
        print dummy    
        itemnr += 1
    Loop
    close(tmp)

    ' table header
    print "<table class='sortable' id='datatable'>"
    print "  <thead><tr>"
    print "     <th width=20px;>"
    print "     <div class='trdropdown'><button class='trdropbtn'></button><div class='trdropdown-content'>"
 
    ' create html table dropdown filter on fieldname
    dummy = ""
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                dummy += record.fieldname(i) + ","
                print "         <a href='' onclick=" + chr$(34) + "localStorage.setItem('tdelement', '" + str(i + 1) + "')" + chr$(34) + ";>" + record.fieldname(i) + "</a>"
            end if
        end with
    next i
    print "     </div></div>"

    ' create table header count nr fields
    dummy = ""
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                dummy += record.fieldname(i) + ","
                print "     <th>" + record.fieldname(i) + "</th>"
                fieldnr += 1
            end if
        end with
    next i
    print "  </tr></thead>"
    print "  <tr>"
    print "      <td></td>"

    ' output fields and values to html
    for i as integer = 0 to recnr
        if cnt = fieldnr then
            print "     <tr>"
            print "        <td></td>"
            cnt = 0
        end if
        with record
            print "        <td>" + record.fieldvalue(i) & "</td>"
        end with
        cnt += 1
        if cnt = fieldnr then
            print "     </tr>"
        end if
    next i
    
    ' table footer
    print "</table>"

    ' get template for footer, css, and javacript    
    tmp = readfromfile(exepath  + "\templates\footer.html")
    Do Until EOF(tmp)
        Line Input #tmp, dummy
        print dummy    
        itemnr += 1
    Loop
    close(tmp)

    ' todo needs more accurate count for fields and records current workaround
    logentry("notice", "html export nr record(s) " & ((recnr + 1) / fieldnr))

    return true

end function

function listjson(needle as string = "") as boolean
    dim dummy   as string = ""
    dim fieldnr as integer = 0
    dim cnt     as integer = 1

    ' count nr fields
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                dummy += record.fieldname(i) + ","
                fieldnr += 1
            end if
        end with
    next i
    dummy = ""

    print "["
    for i as integer = 0 to recnr
        with record
            if instr(record.fieldname(i), "json_object") > 0 then
                if i <> recnr then
                    print record.fieldvalue(i) + ","
                else
                    print record.fieldvalue(i)
                end if
            else
                ' parse non sqlite output

                ' todo implement rest of json escaping
                'Backspace is replaced with \b
                'Form feed is replaced with \f
                'Newline is replaced with \n
                'Carriage return is replaced with \r
                'Tab is replaced with \t
                'Backslash is replaced with \\
                record.fieldvalue(i) = replace(record.fieldvalue(i), "\", "\\")
                'Double quote is replaced with \"
                record.fieldvalue(i) = replace(record.fieldvalue(i), chr$(34), "\" + chr$(34))

                if cnt = fieldnr then
                    dummy += chr$(34) + record.fieldname(i) + chr$(34) + ":" + chr$(34) + record.fieldvalue(i) + chr$(34) + "}," + chr$(13) + chr$(10)
                    cnt = 1
                else
                    if cnt = 1 then
                        dummy += "{"
                    end if
                    dummy += chr$(34) + record.fieldname(i) + chr$(34) + ":" + chr$(34) + record.fieldvalue(i) + chr$(34) + ","
                    cnt += 1
                end if
            end if
        end with
    next i
    if dummy <> "" then
        print mid(dummy, 1, len(dummy) - 3)
    end if
    print "]"
    ' todo needs more accurate count for fields and records current workaround
    logentry("notice", "json export nr record(s) " & ((recnr + 1) / fieldnr))

    return true

end function

function listxml(dbname as string = "", tbname as string = "") as boolean

    dim dummy   as string = ""
    dim fieldnr as integer = 1
    dim cnt     as integer = 1

    ' setup header xml
    print "<?xml version='1.0' encoding='UTF-8'?>"
    dbname = replace(dbname, "\", "_")
    print "<" + mid(dbname, 1, instr(dbname, ".") - 1) + ">"
    print space(len(dbname));"<" + tbname + ">"
 
    ' count nr fields
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                dummy += record.fieldname(i) + ","
                fieldnr += 1
            end if
        end with
    next i

    ' output fields and values to xml
    for i as integer = 0 to recnr
        if cnt = fieldnr then
            print space(len(dbname));"<" + tbname + ">"
            cnt = 1
        end if
        with record
            ' todo figure out why starting from 0 causes empty record
            record.fieldname(i) = replace(record.fieldname(i), " ", "_")
            ' sanitize xml values
            record.fieldvalue(i) = replace(record.fieldvalue(i), " & ", " &amp; ")
            record.fieldvalue(i) = replace(record.fieldvalue(i), ">", "&gt;")
            record.fieldvalue(i) = replace(record.fieldvalue(i), "<", "&lt;")
            print space((len(dbname) + len(tbname)) + 1);"<" + record.fieldname(i) + ">" + record.fieldvalue(i) + "</" + record.fieldname(i) + ">"
        end with
        cnt += 1
        if cnt = fieldnr then
            print space(len(dbname));"</" + tbname + ">"
        end if
    next i
    
    ' xml end
    print "</" + mid(dbname, 1, instr(dbname, ".") - 1) + ">"

    ' todo needs more accurate count for fields and records current workaround
    logentry("notice", "xml export nr record(s) " & ((recnr + 1) / (fieldnr - 1)))

    return true

end function

function listrecords(needle as string = "") as boolean

    dim dummy   as string = ""
    dim fieldnr as integer = 0
    dim cnt     as integer = 1

    ' count nr fields
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                dummy += record.fieldname(i) + ","
                fieldnr += 1
            end if
        end with
    next i
    dummy = ""

    for i as integer = 0 to recnr
        with record
            print record.fieldname(i) + " = " + replace(record.fieldvalue(i), ",", "," + chr$(13) + chr$(10))
            cnt += 1
        end with
        if cnt > fieldnr then
            print "------------------------------------------------------" & _ 
                  "[" & format(fix(i / fieldnr) + 1, "000000") & "]"
            cnt = 1
        end if    
    next i
    logentry("notice", "listrecords nr record(s) " & ((recnr + 1) / fieldnr))

    return true

end function

' main
dim itm        as string
dim inikey     as string
dim inival     as string
dim inifile    as string = exepath + "\conf\" + "conf.ini"
dim f          as long
dim htmloutput as string

' init mp3 cover
dim nocover     as string = ""
dim tempfolder  as string = exepath + "\cover"
dim filename    as string
dim itemnr      as integer = 0
dim listitem    as string
dim maxitems    as integer = 0

' init app overwrite by commandline or config file
if FileExists(inifile) = false then
    logentry("error", inifile + "file does not excist")
else 
    f = readfromfile(inifile)
    Do Until EOF(f)
        Line Input #f, itm
        if instr(1, itm, "=") > 1 then
            inikey = trim(mid(itm, 1, instr(1, itm, "=") - 2))
            inival = trim(mid(itm, instr(1, itm, "=") + 2, len(itm)))
            select case inikey
                case "locale"
                    locale = inival
                case "logtype"
                    logtype = inival
                case "usecons"
                    usecons = inival
                'case "htmloutput"
                '    htmloutput = inival
            end select
            'print inikey + " - " + inival
        end if    
    loop
    close(f)    
end if    

' init basic commandline parser
' via https://www.freebasic.net/forum/viewtopic.php?t=31889 code by coderJeff
dim i               as integer = 1
dim runsqlquery     as boolean = false
dim runlistrecords  as boolean = false
dim dummy           as string = ""

' parse if no commandline options are present
select case true
    case len(command(1)) = 0
        displayhelp(locale)
        logentry("terminate", "normal temination ")
end select

exectime(exectimer, "set")

' parse if commandline options are present
while i < __FB_ARGC__
	select case left(command(i), 1)
        case "-"
            select case command(i)
                case "-h", "-help", "--help", "-man"
                    displayhelp(locale)
                    logentry("terminate", "normal temination ")
                case "-v", "-ver"
                    print appname + " version " + exeversion
                    ' todo odd jumps to line 472 and resumes execution disregarding end
                    logentry("terminate", "normal temination ")
                case else 
                    logentry("fatal", "invalid switch '" & command(i) & "'")
            end select
        case "/"
            select case command(i)
                case "/?"
                    displayhelp(locale)
                    logentry("terminate", "normal temination ")
                case else
                    logentry("fatal", "invalid switch '" & command(i) & "'")
            end select
	end select
    select case i
        case 1
            if FileExists(command(1)) then
                ' nop
            else
                if instr(command(1), ".") <> 0 and (instr(command(1), ".db") = 0 and instr(command(1), ".sqlite") = 0) then 
                    logentry("fatal", "file not found or missing.. '" & command(i) & "'")
                end if
            end if
            select case true
                case instr(command(1), ".sqlite") > 0
                    if command(2) = "" then
                        logentry("fatal", "missing query or option.. '" & command(i) & "'")
                    end if
                case instr(command(1), ".db") > 0
                    if command(2) = "" then
                        logentry("fatal", "missing query or option.. '" & command(i) & "'")
                    end if
                case instr(command(1), ".xml") > 0
                        if len(command(2)) > 0 and command(2) <> "fts" then
                            logentry("fatal", "please specify correct parameter ex. fts ... " + command(2))
                        end if
                        xml2sql(command(1), "", "", command(2))
                        logentry("terminate", "xml2sql duration " + exectime(exectimer, "stop"))
                case instr(command(1), ".csv") > 0
                        if len(command(2)) > 0 and command(2) <> "fts" then
                            logentry("fatal", "please specify correct parameter ex. fts ... " + command(2))
                        end if
                        csv2sql(command(1), "", command(2))
                        logentry("terminate", "csv2sql duration " + exectime(exectimer, "stop"))
                case instr(command(1), ".json") > 0
                        if len(command(2)) > 0 and command(2) <> "fts" then
                            logentry("fatal", "please specify correct parameter ex. fts ... " + command(2))
                        end if
                        json2sql(command(1), "", command(2))
                        logentry("terminate", "json2sql duration " + exectime(exectimer, "stop"))
                case instr(command(1), ".mht") > 0
                        mhtconvert(command(1))
                        wordwrap2file(command(1), swp)
                        logentry("terminate", "mhtconvert duration " + command(1) + " " + exectime(exectimer, "stop"))
                ' eperimental todo better intergration with list and 2sql routines
                case instr(command(1), ".srt") > 0
                    ReDim srtData(0)   As String
                    ReDim startTime(0) As String
                    ReDim endTime(0)   As String
                    dim tbname         as string

                    srt2sql(command(1), srtData(), startTime(), endTime())
                    'recnr = 0
                    ' filter out ext
                    tbname = left(command(1), instrrev(command(1), ".") - 1)
                    ' filter out preceding path if present
                    tbname = lcase(mid(tbname, instrrev(tbname, "\") + 1))
                    ' filter out space
                    tbname = replace(tbname, " ", "")    

                    print "begin transaction;"
                    ' create table defintion
                    'if tabletype = "fts" then
                    '    print "create virtual table if not exists '" + tbname + "' using fts5("        
                    'else
                        print "create table if not exists '" + tbname + "' ("
                    'end if
                    Print "'file'        text,"
                    Print "'subtitlenr'  text,"
                    Print "'starttime'   text,"
                    Print "'endtime'     text,"
                    Print "'text'        text"
                    print ");"

                    For x As Integer = 0 To ubound(srtData)
                        'Print "Subtitle    " & (x + 1)
                        'Print "Start time: " & startTime(x)
                        'Print "End time:   " & endTime(x)
                        'Print "text:       " & mid(srtData(x), instr(srtData(x), "|") + 1, len(srtData(x)) - 4)
                        'Print
                        srtData(x) = replace(srtData(x), "'", "''")
                        srtData(x) = mid(srtData(x), instr(srtData(x), "|") + 1, len(srtData(x)) - 4)
                        srtData(x) = replace(srtData(x), "|", "\n")
                        Print "insert into '" + tbname + "' values ('" + command(1) + "','" + str(x + 1) + "','" & startTime(x)_ 
                                          + "','" & endTime(x) + "','" + srtData(x) + "');" 

                    /'
                        ' srt record
                        redim preserve record.fieldname (0 to recnr + 3)
                        redim preserve record.fieldvalue(0 to recnr + 3)
                        record.fieldname (recnr)     = "subtitlenr"
                        record.fieldvalue(recnr)     = str(x + 1)
                        record.fieldname (recnr + 1) = "starttime"
                        record.fieldvalue(recnr + 1) = str(startTime(x))
                        record.fieldname (recnr + 2) = "endtime"
                        record.fieldvalue(recnr + 2) = str(endTime(x))
                        record.fieldname (recnr + 3) = "text"
                        record.fieldvalue(recnr + 3) = mid(srtData(x), instr(srtData(x), "|") + 1, len(srtData(x)) - 4)
                        recnr += 3
                    '/
                    Next
                    print "commit;"

                    'listrecords()
                    logentry("terminate", "srt2sql duration " + exectime(exectimer, "stop"))

                ' eperimental todo use as text analysis for sqlite fts
                case instr(command(1), ".txt") > 0
                        if len(command(2)) > 0 and (command(2) <> "index" and command(2) <> "text") then
                            logentry("fatal", "please specify correct parameter ex. index ..." + command(2))
                        end if
                        ' todo better implementation of fts see case 2    
                        if command(2) = "index" then
                            Print "insert into '" + "dictionary" + "' values ('" + command(1) + "','" + dictonary(command(1), wc) + "');" 
                        else
                            txt2sql(command(1), command(2), command(3))
                        end if
                        logentry("terminate", "txt2sql duration " + exectime(exectimer, "stop"))
                case instr(command(1), ":") > 0
                    if checkpath(command(1)) = false then
                        logentry("fatal", "please specify a valid file or path.. '" & command(i) & "'")
                    end if
                    if len(command(2)) = 0 then
                        logentry("fatal", "please specify filespec ex. *.mp3, *.txt, etc.. '" & command(i) & "'")
                    end if
                    if command(2) = "folderinfo" then
                        print
                        print "label         : "; getdrivelabel(left(command(1), 1) + ":\")
                        Print "total capacity: "; getdrivestorage(left(command(1), 1) + ":\", "capacity"); " bytes " + convertbytesize(getdrivestorage(left(command(1), 1) + ":\", "capacity"))
                        print "free space    : "; getdrivestorage(left(command(1), 1) + ":\", "space"); " bytes " +  convertbytesize(getdrivestorage(left(command(1), 1) + ":\", "space"))
                        print
                        ReDim As String ordinance(0)
                        dim gettpath as string = command(1) + "\"
                        getfolders(gettpath + "*", ordinance())
                        dim offset as integer = len(arraylongestvalue(ordinance()))
                        For x As Integer = 1 To UBound(ordinance)
                            Print ordinance(x) + space(offset * 1.1 - Len(ordinance(x))) +_
                                  format(FileDateTime(gettpath + ordinance(x)), "dd-mm-yyyy hh:mm") +_
                                  " " + convertbytesize(foldersize(gettpath + ordinance(x)))  
                        Next
                        print
                        getfilesfromfolder(gettpath + "*", ordinance())
                        offset = len(arraylongestvalue(ordinance()))
                        For x As Integer = 1 To UBound(ordinance)
                            Print ordinance(x) + space(offset * 1.1 - Len(ordinance(x))) +_
                            format(FileDateTime(gettpath + ordinance(x)), "dd-mm-yyyy hh:mm") +_
                            " " & convertbytesize(filelen(gettpath + ordinance(x)))
                        Next
                        print
                        Print "Folder size: " & convertbytesize(foldersize(gettpath)) & " bytes"
                        logentry("terminate", "normal termination show folder info")
                    end if
                    ' todo move catalog to seperate function
                    if command(2) = "catalog" then
                        ReDim As String ordinance(0)
                        dim gettpath as string = command(1) + "\"
                        getfolders(gettpath + "*", ordinance())

                        ' metric record
                        recnr = 0
                        redim preserve record.fieldname (0 to 4)
                        redim preserve record.fieldvalue(0 to 4)
                        record.fieldname (recnr)     = "label"
                        record.fieldvalue(recnr)     = getdrivelabel(left(command(1), 1) + ":\")
                        record.fieldname (recnr + 1) = "capacity"
                        record.fieldvalue(recnr + 1) = str(getdrivestorage(left(command(1), 1) + ":\", "capacity"))
                        record.fieldname (recnr + 2) = "space"
                        record.fieldvalue(recnr + 2) = str(getdrivestorage(left(command(1), 1) + ":\", "space"))
                        record.fieldname (recnr + 3) = "foldersize"
                        record.fieldvalue(recnr + 3) = str(foldersize(gettpath))
                        recnr += 3
                        select case command(3)
                            case "csv"
                                listcsv()
                            case "json"
                                print "{ " + chr$(34) + "archive" + chr$(34) + ":"
                                listjson()
                                print ", " + chr$(34) + "data" + chr$(34) + ":"
                                listjson()
                                print "}"
                                logentry("terminate", "catalog json export")
                            case "html"
                                logentry("fatal", "catalog html export not supported.. '" & command(i) & "'")
                            case "sql"
                                ' todo needs beter handeling of inital db creation
                                print "begin transaction;"
                                    ' empty table needed to prevent delete throwing error when table is created first time is handy for setting up integer field type    
                                    print "create table if not exists 'archive' ("
                                    print "'label'               text,"
                                    print "'capacity'            integer,"
                                    print "'space'               integer,"
                                    print "'foldersize'          integer"
                                    print ");"
                                    print "delete from archive where label='" & getdrivelabel(left(command(1), 1) + ":\") & "';"
                                print "commit;"
                                listsql("archive")
                                print "begin transaction;"
                                    ' empty table needed to prevent delete throwing error when table is created first time is handy for setting up integer field type    
                                    print "create table if not exists 'data' ("
                                    print "'label'               text,"
                                    print "'folder'              text,"
                                    print "'date'                text,"
                                    print "'size'                integer"
                                    print ");"
                                    print "delete from data where label='" & getdrivelabel(left(command(1), 1) + ":\") & "';"
                                print "commit;"
                            case "xml"
                                ' todo adhere to xml element rules
                                ' see https://stackoverflow.com/questions/442529/is-there-a-standard-naming-convention-for-xml-elements
                                listxml(replace(replace(left(command(1),instrrev(command(1), "\") - 1), "\", "_"), ":", ""), "archive")
                        end select

                        ' data record(s)
                        recnr = 0
                        For x As Integer = 1 To UBound(ordinance)
                            redim preserve record.fieldname (0 to recnr + 4)
                            redim preserve record.fieldvalue(0 to recnr + 4)
                            record.fieldname (recnr)     = "label"
                            record.fieldvalue(recnr)     = getdrivelabel(left(command(1), 1) + ":\")
                            record.fieldname (recnr + 1) = "folder"
                            record.fieldvalue(recnr + 1) = ordinance(x)
                            record.fieldname (recnr + 2) = "date"
                            record.fieldvalue(recnr + 2) = format(FileDateTime(gettpath + ordinance(x)), "yyyy-mm-dd hh:mm")
                            record.fieldname (recnr + 3) = "size"
                            record.fieldvalue(recnr + 3) = str(foldersize(gettpath + ordinance(x)))
                            recnr += 4
                        next x
                    end if
                    SELECT case command(3)
                        case "csv", "json", "html", "sql", "xml"
                            ' note somehow freebasic has an issue with the wildcard *
                            ' dir2file needed in all cases....
                            dir2file(command(1), command(2), command(3), command(4))
                            if command(2) = "catalog" then
                                dummy = "data"
                            else
                                dummy = replace(mid(command(1), instrrev(command(1), "\") + 1), " ", "")
                            end if
                            select case command(3)
                                case "csv"
                                    listcsv()
                                case "json"
                                    listjson()
                                case "sql"
' todo needs work with switches exif, fts, content and index
'if (command(5) = "utf8" or command(5) = "utf16")
                                    if len(command(5)) > 0 and command(5) <> "fts" then
                                        logentry("fatal", "please specify correct parameter ex. fts ... " + command(5))
                                    end if
                                    if len(command(4)) > 0 and command(4) <> "fts" and (command(4) <> "exif" and command(4) <> "index") then
                                        logentry("fatal", "please specify correct parameter ex. fts ... " + command(5))
                                    end if
                                    if command(4) = "fts" then
                                        listsql(dummy, command(4), command(6))
                                    else
                                        listsql(dummy, command(5), command(6))
                                    end if
                                case "xml"
                                    ' todo adhere to xml element rules
                                    ' see https://stackoverflow.com/questions/442529/is-there-a-standard-naming-convention-for-xml-elements
                                    listxml(replace(replace(left(command(1),instrrev(command(1), "\") - 1), "\", "_"), ":", ""), dummy)
                            END select
                            logentry("notice", "dir2" + command(3) + " duration " + exectime(exectimer, "stop"))
                        case "cover"
                            if instr(command(2), ".mp3") > 0 then
                                ' export covers to jpeg or png file(s)
                                mkdir (tempfolder) ' create export folder regardless
                                print "scanning and exporting mp3 covers(s)...."
                                if instr(command(1), ".mp3") > 0 then
                                    getmp3cover(command(1), filename)
                                    itemnr = 1
                                else
                                    createlist(command(1), ".mp3", "cover")
                                    'f = freefile
                                    open "cover.tmp" for input as #20
                                    Do Until EOF(20)
                                        Line Input #20, listitem
                                        filename = lcase(mid(listitem, instrrev(listitem, "\") + 1))
                                        filename =  lcase(mid(filename, 1, instrrev(filename, ".") - 1))
                                        if getmp3cover(listitem, filename) then
                                            itemnr += 1
                                        else
                                            nocover = nocover + "no cover art found in " + filename + chr$(13) + chr$(10)
                                            csv = csv + chr$(34) + command(1) + "\" + filename + chr$(34) + ",0,0" + chr$(13) + chr$(10)
                                        end if
                                        listitem = ""
                                        maxitems += 1
                                    loop
                                    close(20)
                                    ' strip final carrige return csv
                                    csv = mid(csv, 1, len(csv) - 2)
                                    ' cleanup listplay files
                                    delfile(exepath + "\cover.tmp")
                                    delfile(exepath + "\cover.lst")
                                end if
                                ' report to command line
                                print nocover
                                if thumbnail = "" then
                                    print "no thumbnail(s) found in scanned files"
                                else
                                    print thumbnail
                                end if
                                if layout = "" then
                                    print "all scanned file(s) are sqare"
                                else
                                    print layout
                                end if
                                print "finished scanning " & maxitems & " file(s)"
                                print "exported " & itemnr & " covers(s) to " + tempfolder
                                ' export results as csv
                                f = freefile
                                open "mp3cover.csv" for output as f
                                    print #f, csv
                                close(f)
                                print "created report mp3cover.csv in " + exepath
                                print "total duration operation(s) " + exectime(exectimer, "stop")
                                logentry("terminate", "export mp3 album covers duration " + exectime(exectimer, "stop"))
                            else
                                logentry("fatal", "only supports mp3 files .. '" & command(i) & "'")
                            end if
                        case else
                            logentry("fatal", "please specify a valid export file type ex. sql, json, etc or option ..'" & command(3) & "'")
                    end select
                    logentry("terminate", "normal termination created " + command(3))
                case else
                    'if FileExists(command(1)) then
                        logentry("fatal", "file not supported.. '" & command(i) & "'")
                    'else
                    '    logentry("fatal", "file not found or missing.. '" & command(i) & "'")
                    'end if
            end select
        case 2
            select case true
                case command(2) = "index"
                    if command(3) <> "" then
                        ' todo becomes an issue with larger tables 10MB and up
                        sel =  "begin transaction;"
                        sel += " create virtual table if not exists fts5terms using fts5vocab('" + command(3) + "', 'row');"
                        sel += " select term, cnt from fts5terms where term GLOB '[A-Za-z]*' AND term NOT GLOB '*[0-9]*';"
                        sel += " commit; "
                        runsqlquery = true
                    else
                        logentry("fatal","please specify a valide table to index ...")
                    end if
                case command(2) = "showtables"
                    sel = "select name from sqlite_schema where type ='table' and name not like 'sqlite_%'"
                    runsqlquery = true
                    runlistrecords = true
                case command(2) = "showfields"
                    if command(3) = "" then
                        logentry("fatal", "missing table name.. '" & command(i) & "'")
                    end if
                    sel = "select sql from sqlite_schema where name = '" + command(3) + "'"
                    runsqlquery = true
                    runlistrecords = true
                 case instr(command(2), ".sql") > 0 and (instr(command(1), ".db") > 0 or instr(command(1), ".sqlite") > 0)
                    if FileExists(command(2)) = false then
                        logentry("fatal", "file not found or missing.. '" & command(2) & "'")
                    end if
                    sel = ""    
                    f = readfromfile(command(2))
                    Do Until EOF(f)
                        Line Input #f, itm
                        sel += itm + chr$(13) + chr$(10)
                    loop
                    close(f)
                    runsqlquery = true
                    logentry("notice", "imported sql '" & command(2) & "' added to or created " + command(1) + " duration " + exectime(exectimer, "stop"))
                 case instr(command(2), ".query") > 0 and (instr(command(1), ".db") > 0 or instr(command(1), ".sqlite") > 0)
                    if FileExists(command(2)) = false then
                        logentry("fatal", "file not found or missing.. '" & command(2) & "'")
                    end if
                    sel = ""    
                    f = readfromfile(command(2))
                    Do Until EOF(f)
                        Line Input #f, itm
                        ' filter out single line comment
                        if left(trim(itm), 1) = "#" then
                            'nop
                        else
                            sel += itm
                        end if
                    loop
                    close(f)
                    runsqlquery = true
                    ' check for export type
                    if command(3) = "" then
                        runlistrecords = true
                    end if
                    logentry("notice", "imported sql query '" & command(2) & "'")
                case instr(command(3), "json") > 0
                    sel = command(2)
                    ' todo phase out by getting fieldlist and adding to query    
                    if instr(sel, "select *") > 0 or instr(sel, "select*") > 0then
                        logentry("fatal", "export json does not support select * from please specify fieldnames")
                    end if
                    ' build json query
                    dummy = "select json_object("
                    ReDim As String ordinance(0)
                    explode(trim(mid(sel, 7, instr(sel, "from") - 7)), ",", ordinance())
                    For x As Integer = 1 To UBound(ordinance)
                        ' catch fieldname rename via as
                        'if instr(ordinance(x), " as ") > 0 then
                        'ordinance(x) = mid(ordinance(x), instr(ordinance(x), " as ") + 4)
                        'end if
                        if x <> UBound(ordinance) then
                            dummy += "'" + trim(ordinance(x)) + "'," + trim(ordinance(x) + ",")
                        else
                            dummy += "'" + trim(ordinance(x)) + "'," + trim(ordinance(x))
                        end if
                    Next
                    dummy += ") " + mid(sel, instr(sel, "from")) + ";"
                    sel = dummy
                    runsqlquery = true
                case len(command(2)) > 0
                    sel = command(2)
                    runsqlquery = true
                    if command(3) = "" then
                        runlistrecords = true
                    end if
            end select    
        case 3
            select case true
                case instr(command(3), "checkfile") > 0
                    if command(4) = "" then
                        logentry("fatal", "checkfile specify field name with file path.. '" & command(i) & "'")
                    else
                        'checkfile(command(4))
                        listcsv(command(4), true)
                    end if
                case instr(command(3), "csv") > 0
                    listcsv()
                case instr(command(3), "html") > 0
                    listhtml()
                case instr(command(3), "json") > 0
                    listjson()
                case instr(command(3), "sql") > 0
                    listsql(command(1))
                case instr(command(3), "xml") > 0
                    ' parse out table name from select and pass as xml node
                    ' todo needs better handling
                    dummy = mid(sel, instr(sel, " from ") + 6, instr(instr(sel, " from ") + 6, sel, " ") - (instr(sel, " from ") + 6))
                    listxml(command(1), dummy)
            end select                
    end select

    if runsqlquery then
        ' open db
        fn = command(1)
        rc = sqlite3_open(fn, @db)
        If rc Then
            sqlite3_close(db)
            logentry("fatal", "error opening database " & *sqlite3_errmsg(db))
        End If
        ' query db
        recnr = 0
        rc = sqlite3_exec(db, sel, @sqlitegetrecord, 0, @zErrMsg)
        If rc Then
            print sel
            Print "SQL error: ";*zErrMsg
            sqlite3_free(zErrMsg)
        End If
        recnr = recnr - 1
        sqlite3_close(db)
        runsqlquery = false
    end if

    ' show results query
    if runlistrecords then
        listrecords()
        runlistrecords = false
        logentry("notice", "query duration " + exectime(exectimer, "stop"))
    end if    

	i += 1
wend

sqlite3_close(db)

End