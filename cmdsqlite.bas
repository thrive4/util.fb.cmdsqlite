' import export various file formats csv, json, html, sql and xml
' to and from a sqlite database by thrive4 sept 2023
' more info see: https://github.com/thrive4/util.fb.cmdsqlite

declare function listrecords(needle as string = "") as boolean
#include once "sqlite3.bi"
#include once "windows.bi"
#include once "utilfile.bas"
#cmdline "app.rc"

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

function listsql(needle as string = "") as boolean

    dim dummy   as string = ""
    dim fieldnr as integer = 0
    dim cnt     as integer = 1

    ' create table
    print "begin transaction;"
    print "create table if not exists '" + needle + "' ("

    ' get fieldnames aka header
    for i as integer = 0 to recnr
        with record
            if instr(dummy, record.fieldname(i)) = 0  then
                if i < recnr then
                    dummy += "'" + record.fieldname(i) + "'" + space(20 - len(record.fieldname(i))) + "text," + chr$(13) + chr$(10)
                else
                    dummy += "'" + record.fieldname(i) + "'" + space(20 - len(record.fieldname(i))) + "text," + chr$(13) + chr$(10)
                end if
                fieldnr += 1
            end if
        end with
    next i
    print mid(dummy, 1, len(dummy) - 3)
    print ");"

    ' get fieldvalues aka data
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
    dim         as integer itemnr = 1, i = 1, n = 1, tmp, f

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
'            print record.fieldname(i) + " = " + record.fieldvalue(i)
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
' init app overwrite by commandline or config file
dim itm        as string
dim inikey     as string
dim inival     as string
dim inifile    as string = exepath + "\conf\" + "conf.ini"
dim f          as integer
dim htmloutput as string

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
                case "htmloutput"
                    htmloutput = inival
            end select
            'print inikey + " - " + inival
        end if    
    loop    
end if    

' basic commandline parser
' via https://www.freebasic.net/forum/viewtopic.php?t=31889 code by coderJeff
dim i               as integer = 1
dim runsqlquery     as boolean = false
dim runlistrecords  as boolean = false
dim dummy           as string = ""

'print "cmd1 " + command(1)
'print "cmd2 " + command(2)
'print "cmd3 " + command(3)

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
            select case true
                case instr(command(1), ".db") > 0
                    if command(2) = "" then
                        logentry("fatal", "missing query or option.. '" & command(i) & "'")
                    end if
                case instr(command(1), ".xml") > 0
                    if FileExists(command(1)) then
                        xml2sql(command(1), "")
                        logentry("terminate", "xml2sql duration " + exectime(exectimer, "stop"))
                    else
                        logentry("fatal", "file not found or missing.. '" & command(i) & "'")
                    end if
                case instr(command(1), ".csv") > 0
                    if FileExists(command(1)) then
                        csv2sql(command(1), "")
                        logentry("terminate", "csv2sql duration " + exectime(exectimer, "stop"))
                    else
                        logentry("fatal", "file not found or missing.. '" & command(i) & "'")
                    end if
                case instr(command(1), ".json") > 0
                    if FileExists(command(1)) then
                        json2sql(command(1), "")
                        logentry("terminate", "json2sql duration " + exectime(exectimer, "stop"))
                    else
                        logentry("fatal", "file not found or missing.. '" & command(i) & "'")
                    end if
                ' eperimental todo use as text analysis for sqlite fts5
                'case instr(command(1), ".txt") > 0
                '        dictonary(command(1), wc)
                case instr(command(1), ":") > 0
                    if checkpath(command(1)) = false then
                        logentry("fatal", "please specify a valid path.. '" & command(i) & "'")
                    end if            
                    if len(command(2)) = 0 then
                        logentry("fatal", "please specify filespec ex. *.mp3.. '" & command(i) & "'")
                    end if
                    SELECT case command(3)
                        case "csv", "json", "html", "sql", "xml"
                            ' note somehow freebasic has an issue with the wildcard *
                            dir2file(command(1), command(2), command(3), htmloutput)
                            dummy = replace(mid(command(1), instrrev(command(1), "\") + 1), " ", "")
                            select case command(3)
                                case "csv"
                                    listcsv()
                                case "json"
                                    listjson()
                                case "sql"
                                    listsql(dummy)
                                case "xml"
                                    ' todo adhere to xml element rules
                                    ' see https://stackoverflow.com/questions/442529/is-there-a-standard-naming-convention-for-xml-elements
                                    listxml(replace(replace(left(command(1),instrrev(command(1), "\") - 1), "\", "_"), ":", ""), dummy)
                            END select
                            logentry("notice", "dir2" + command(3) + " duration " + exectime(exectimer, "stop"))
                        case else
                            logentry("fatal", "please specify a valid export file type.. '" & command(i) & "'")
                    end select
                    logentry("terminate", "normal termination created " + command(3))
                case else
                    if FileExists(command(1)) then
                        logentry("fatal", "file not supported.. '" & command(i) & "'")
                    else
                        logentry("fatal", "file not found or missing.. '" & command(i) & "'")
                    end if
            end select
        case 2
            select case true
                case instr(command(2), "showtables") > 0
                    sel = "select name from sqlite_schema where type ='table' and name not like 'sqlite_%'"
                    runsqlquery = true
                    runlistrecords = true
                case instr(command(2), "showfields") > 0
                    if command(3) = "" then
                        logentry("fatal", "missing table name.. '" & command(i) & "'")
                    end if
                    sel = "select sql from sqlite_schema where name = '" + command(3) + "'"
                    runsqlquery = true
                    runlistrecords = true
                 case instr(command(2), ".sql") > 0 and instr(command(1), ".db") > 0
                    if FileExists(command(2)) = false then
                        logentry("fatal", "file not found or missing.. '" & command(2) & "'")
                    end if
                    sel = ""    
                    f = readfromfile(command(2))
                    Do Until EOF(f)
                        Line Input #f, itm
                        sel += itm
                    loop
                    close(f)
                    runsqlquery = true
                    logentry("notice", "imported sql '" & command(2) & "' added to or created " + command(1) + " duration " + exectime(exectimer, "stop"))
                 case instr(command(2), ".query") > 0 and instr(command(1), ".db") > 0
                    if FileExists(command(2)) = false then
                        logentry("fatal", "file not found or missing.. '" & command(2) & "'")
                    end if
                    sel = ""    
                    f = readfromfile(command(2))
                    Do Until EOF(f)
                        Line Input #f, itm
                        sel += itm
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
                case else
                    if instr(command(2), "showtables") = 0 and instr(command(2), "showfields") = 0 then
                        logentry("fatal", "please specify a valid export file type.. '" & command(i) & "'")
                    end if
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