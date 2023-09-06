## cmdsqlite
basic import / export util written in freebasic with sqlite3\
supported file types or extensions:\
.csv, .db, .json, .html, .sql, .xml\
\
Allows for export of data in sqlite database via sql query,\
or a folder plus filespec, to csv, json, html(table), sql and xml\
and import of csv, json and xml via conversion to sql.\
\
Special support for .mp3 or .jpg this will extract the\
tag info and or create media links in the generated html table\
plus checkfile which verifies if file excists according to path\
and file in specified field.\
\
See included help, or tutorial.txt in data folder, for more details\
cmdsqlite.exe /?  (or -h, -help, etc)\
Note: [folder] [filespec] folders are scanned recursively for filespec
## usage
- basic\
cmdsqlite.exe [dbname] [query]\
example: cmdsqlite.exe game.db "select name from game where name like 'a%'"\
cmdsqlite.exe [folder] [filespec] [exporttype]\
example: cmdsqlite.exe g:\data\images\classic *.jpg csv

- verfiy if file excists according to path and file in specified field\
[dbname] [query] checkfile [field name]\
checkfile returns -1 if the file exists, otherwise zero 0

- export to html(table)\
cmdsqlite.exe game.db "select name, developer from game" html\
exports query result to an html sortable table (using templates)\
! more then 120 records can cause slow downs in browser when sorting

- export to html(table) via folder\
cmdsqlite.exe g:\data\mp3\classic *.mp3 html\
exports all files and subsequent folders in 'g:\data\mp3\classic'\
to an html sortable table (using templates)

- export to json\
cmdsqlite.exe game.db "select name, developer from game" json\
equivelent of:\
select json_object('name', name, 'developer', developer) from game" json

- exports to csv\
cmdsqlite.exe game.db "select name, developer from game" csv

- import via sql of csv, json or xml\
cmdsqlite.exe [filename].csv\
creates a [filename].sql\
can be verified and the imported to sqlite database via:\
cmdsqlite.exe [dbname] [.sql]

- basic info database and tables\
cmdsqlite.exe [dbname] showtables\
displays table names contained in [dbname]\
cmdsqlite.exe [dbname] showfields [tablename]\
displays fieldnames contained in [tablename]

## configuration
options via \conf\conf.ini\
[output]\
' additional parsing per filetype mp3, jpg\
' options: default, extra\
htmloutput      = extra
## requirements
sqlite.dll 32-bit DLL (x86) for SQLite version 3.43.0.\
https://www.sqlite.org/download.html
## performance (query and data size dependent)
windows 7 / windows 10(1903)\
ram usage ~10MB / 10MB \
handles   ~30 / ~50\
threads   1 / 3\
cpu       ~1 (low) / ~2\
tested on intel i5-6600T
## special thanks
tips on commandline parsing via:\
https://www.freebasic.net/forum/viewtopic.php?t=31889 code by coderJeff\
data set vgsales via:\
https://gist.github.com/zhonglism/f146a9423e2c975de8d03c26451f841e