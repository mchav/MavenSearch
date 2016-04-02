@ECHO OFF
SETLOCAL EnableExtensions EnableDelayedExpansion
for /f "delims=" %%i in ('chdir') do SET CWD=%%i

SET APPDIR=%~dp0

CD %APPDIR%

SET TARGET=target\scala-2.11\classes
SET SRC_FILE=src\main\scala\MavenSearch.scala

MKDIR %TARGET%  > nul 2>&1
SET changed=0

FOR /F %%z IN ('DIR /B /O:D %%i%% %NAILGUN_INDICATOR% 2^> nul') DO SET NEWEST=%%z
IF "%NEWEST:~-5%"=="scala" ( SET /A changed=1 )

IF /I "%changed%" EQU "1" ( 
	scalac -d %TARGET% %SRC_FILE% 
)


scala -cp %TARGET% MavenSearch %* 

ENDLOCAL
