#!/usr/bin/env python3
import os
import subprocess
import sys

VALID_FILE_DIR = 'tests/valid'
INVALID_FILE_DIR = 'tests/invalid'
failedFlag = True

# Source: https://code-maven.com/python-capture-stdout-stderr-exit
def run(command):
    proc = subprocess.Popen(command,
        stdout = subprocess.PIPE,
        stderr = subprocess.PIPE,
    )
    stdout, stderr = proc.communicate()
    return proc.returncode, stdout, stderr

for test in os.listdir(VALID_FILE_DIR):
    code, out, err = run(['./team.native', os.path.join(VALID_FILE_DIR, test)])
    if (code != 0):
        print(test + " Expected to pass but it failed")
        failedFlag = False

for test in os.listdir(INVALID_FILE_DIR):
    code, out, err = run(['./team.native', os.path.join(INVALID_FILE_DIR, test)])
    if code == 0:
        print(test + " Expected to fail but it passed")
        failedFlag = False

if failedFlag:
    print("All tests passed")