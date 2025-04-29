@echo off

setlocal EnableDelayedExpansion

set passed=0
set all=0

rem Loop over all .txt files in the current directory
for /R "provided_tests" %%f in (*.txt) do (
    set "txt_file=%%f"
    set "expected_file=%%f.expected"

    rem Check if the corresponding .expected file exists
    if not exist "!expected_file!" (
        echo No expected file for !txt_file!, skipping.
        goto :continue
    )

    set /A all+=1

    rem Run the .txt file through a.out and save the output
    a.out < "!txt_file!" > "!txt_file!.output"

    rem Compare the output with the expected file
    fc /W "!txt_file!.output" "!expected_file!" > nul
    if !errorlevel! == 0 (
        set /A passed+=1
        echo [PASS] !txt_file!
    ) else (
        echo [FAIL] !txt_file!
        fc /W "!txt_file!.output" "!expected_file!"
    )
    echo ---------------------------

    rem Clean up
    del "!txt_file!.output"
)

:continue

echo.
echo Passed !passed! tests out of !all!
echo.