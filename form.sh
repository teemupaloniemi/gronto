#!/bin/bash

read -p "Course name: " COURSE_NAME
read -p "Course code: " COURSE_CODE
PREQ=$(cat data/acm.txt | fzf -i -m --prompt="Search by typing. Use TAB to select all that apply. Quit using ENTER. (prerequisites) >")
OUTC=$(cat data/acm.txt | fzf -i -m --prompt="Search by typing. Use TAB to select all that apply. Quit using ENTER. (outcomes) >")

echo -e "{"
echo -e "  \"title\": $COURSE_NAME,"
echo -e "  \"code\": $COURSE_CODE,"
echo -e "  \"prerequisites\": [ $PREQ ],"
echo -e "  \"outcomes\": [ $OUTC ]"
echo -e "}"
