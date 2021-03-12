#!/usr/bin/env python3
import os
import subprocess

VALID_FILE_DIR = 'tests/valid'
INVALID_FILE_DIR = 'tests/invalid'
failedFlag = True

# Color printing purposes
class bcolors:
    PASS = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

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
        print(bcolors.FAIL + test + " Expected to pass but it failed" + bcolors.ENDC)
        failedFlag = False

for test in os.listdir(INVALID_FILE_DIR):
    code, out, err = run(['./team.native', os.path.join(INVALID_FILE_DIR, test)])
    if code == 0:
        print(bcolors.FAIL + test + " Expected to fail but it passed" + + bcolors.ENDC)
        failedFlag = False

if failedFlag:
    print(bcolors.PASS + "All tests passed" + bcolors.ENDC)