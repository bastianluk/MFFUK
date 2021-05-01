#!/bin/bash

set -ueo pipefail

# Directory with this script contains also the extraction script
TOOLS_DIR="$( dirname "$( which -- "${BASH_SOURCE[0]}" 2>/dev/null )" )"

get_filenames() {
    find . \
        -name '[0-9][0-9]-*-before.md' \
        -or -name '[0-9][0-9]-*-graded.md' \
    | sort \
    | tail -n "$1"
}

EXIT_CODE=0
for FILENAME in $( get_filenames 2 ); do
    echo "==> Checking $FILENAME ..."
    "$TOOLS_DIR/extract_answers.py" <"$FILENAME" || EXIT_CODE=1
done

exit "$EXIT_CODE"
