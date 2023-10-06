scmlit -fbuild -e(bi) build %*
@IF NOT ERRORLEVEL 1 GOTO ok
@ECHO ****    build.bat FAILED!    ****
:ok
