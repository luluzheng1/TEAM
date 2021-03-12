#!/usr/bin/env python3
import sys
import os
import subprocess

VALID_FILE_DIR = 'tests/valid'
INVALID_FILE_DIR = 'tests/invalid'
failedFlag = True

test_suite = []
for dirName, subdirName, fileNames in os.walk("tests"):
    for file in fileNames:
        if "tm" not in file:
            continue
        test_suite.append(os.path.join(dirName, file))

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

for test_file in test_suite:
        code, out, err = run(['./team.native', '{}'.format(test_file)])
        filename = test_file.split("/")[-1]
        if code == 0:
            print(bcolors.PASS + "{:25}==> Passed!".format(filename) + bcolors.ENDC)
        else:
            print(bcolors.FAIL + "{:25}==> {}".format(filename, err.decode('utf-8').rstrip()) + bcolors.ENDC)
            failedFlag = False

if failedFlag:
    print(bcolors.PASS + "All tests passed" + bcolors.ENDC)