#!/bin/bash

let passed=0
let all=0

# Loop over all .txt files in the current directory
for txt_file in provided_tests/*/*.txt; do
    # Skip if no .txt files are found
    [[ -e "$txt_file" ]] || continue

    # Define the expected output file
    expected_file="${txt_file}.expected"
    
    # Check if the corresponding .expected file exists
    if [[ ! -f "$expected_file" ]]; then
        echo "No expected file for $txt_file, skipping."
        continue
    fi

	all=$((all+1))

    # Run the .txt file through a.out and save the output
    ./a.out < "$txt_file" > "${txt_file}.output"

    # Compare the output with the expected file
    if diff -Bw "${txt_file}.output" "$expected_file" > /dev/null; then
		passed=$((passed+1))
        echo "[PASS] $txt_file"
		echo "---------------------------"
    else
        echo "[FAIL] $txt_file"
		diff -Bw "${txt_file}.output" "$expected_file"
        # diff -u "${txt_file}.output" "$expected_file"  # Show detailed diff
		echo "---------------------------"
    fi

    # Clean up
    rm "${txt_file}.output"
done

echo
echo "Passed $passed tests out of $all"
echo