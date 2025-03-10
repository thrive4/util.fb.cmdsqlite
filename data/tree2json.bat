: via https://stackoverflow.com/questions/15990113/cmd-tree-to-json
: tree2json [startfolder] [>file.txt]
@echo off &setlocal
if "%~1"=="" (set "root=.") else set "root=%~1"
set "pre0=                                    "

pushd %root%
echo(data = [
call:dirtree "%CD%" "1" "1"
popd
echo(];
goto:eof

:dirtree
setlocal
call set "pre=%%pre0:~-%~2%%
set /a ccount=%~3
set /a tcount=%~2+2
set /a dcount=0
for /d %%i in (*) do set /a dcount+=1
echo( %pre%{
set "fpath=%~f1"
set "fpath=%fpath:\=/%"
echo(  %pre%"path": "%fpath%",
if %dcount% gtr 0 (
    echo(  %pre%"children": [
    for /d %%i in (*) do (
        for /f %%j in ('call echo "%%dcount%%"') do (
            cd "%%i"
            call:dirtree "%%i" "%tcount%" "%%j"
            cd ..
        )
        set /a dcount-=1
    )
    echo(  %pre%]
)
if %ccount% equ 1 (echo  %pre%}) else echo( %pre%},
endlocal
goto:eof
