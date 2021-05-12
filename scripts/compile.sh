#!/bin/bash
# Authors: Lulu Z.
export PATH=$PATH:/usr/local/opt/llvm/bin
# needed for compilation
CC="gcc"
LIBS="-g -Wall -lpcreposix -lpcre2-8"
# file is command line argument, filename is file without extension
file=$1
filename=$(echo "$file" | cut -f 1 -d ".")

func=$2

create() { 
    # generate code, need to pass in file since it could be modified
    ./team.native -l "$1" > "$filename".ll
    llc "$2".ll
    eval "$CC $LIBS -o $filename.exe $filename.s regex.o fileio.o"
    # echo "$filename.exe created"
}

# SCRIPT BEGINS HERE
if [[ $# -eq 0 ]]
then
    echo "usage: ./compile.sh <file.tm> [func]"
    exit 0
fi

if [ ! -e $file ]
then 
    echo "file $file not found"
    exit 0
fi

if [[ "$func" == "clean" ]]
then
    # echo "cleaning: $filename.s $filename.ll $filename.exe"
    rm "$filename".s "$filename".ll "$filename".exe
    rm -rf "$filename".exe.dSYM
    exit 0
fi

# echo "${reset}compiling: $file"

create "$file" "$filename"

if [[ "$func" == "run" ]]
then
    # echo "running: $filename.exe"
    ./"$filename".exe
fi
