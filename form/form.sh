#!/bin/bash

read -p "Course name (i.e. Funktio-ohjelmointi): " COURSE_NAME
read -p "Course code             (i.e. TIEA341): " COURSE_CODE
read -p "Course credits         (i.e. 5 or 1-5): " COURSE_CREDITS

printf "Search by typing. Use TAB to select all that apply. Quit using ENTER. (prerequisites)\n"
PREQ=$(cat ../data/acm.txt | fzf -i -m --prompt=">" --preview 'python3 path.py {}' --preview-window right,border --layout=reverse --height=70% | tr '\n' ',')
printf "Search by typing. Use TAB to select all that apply. Quit using ENTER. (outcomes)\n"
OUTC=$(cat ../data/acm.txt | fzf -i -m --prompt=">" --preview 'python3 path.py {}' --preview-window right,border --layout=reverse --height=70% | tr '\n' ',')

echo -e "{"
echo -e "  \"title\": \"$COURSE_NAME\","
echo -e "  \"code\": \"$COURSE_CODE\","
echo -e "  \"credits\": \"$COURSE_CREDITS\","
echo -e "  \"prerequisites\": [ $PREQ ],"
echo -e "  \"outcomes\": [ $OUTC ]"
echo -e "}"
