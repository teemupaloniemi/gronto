#!/bin/bash

INPUT_FILE="data/input.json"
STAMP_FILE=".input.json.sha256"
SLEEP_SECONDS=1

make visualize

while true; do
    if [ ! -f "$INPUT_FILE" ]; then
        echo "Error: $INPUT_FILE not found."
        sleep "$SLEEP_SECONDS"
        continue
    fi

    NEW_HASH=$(sha256sum "$INPUT_FILE" | awk '{print $1}')

    if [ -f "$STAMP_FILE" ]; then
        OLD_HASH=$(cat "$STAMP_FILE")
    else
        OLD_HASH=""
    fi

    if [ "$NEW_HASH" != "$OLD_HASH" ]; then
        if make distance solve; then
            echo "$NEW_HASH" > "$STAMP_FILE"
        fi
    fi

    sleep "$SLEEP_SECONDS"
done

