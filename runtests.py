#!/usr/bin/env python3
import sys
import os
import subprocess
import optparse


VALID_FILE_DIR = 'tests/valid'
INVALID_FILE_DIR = 'tests/invalid'


def runValidTests():
    print("\nRunning Valid Tests...\n")
    for file in map(lambda x: "/".join((VALID_FILE_DIR, x)), filter(lambda x: "tm" in x, os.listdir(VALID_FILE_DIR))):
        runFile(file, True)

def runInValidTests():
    print("\nRunning Invalid Tests...\n")
    for file in map(lambda x: "/".join((INVALID_FILE_DIR, x)), filter(lambda x: "tm" in x, os.listdir(INVALID_FILE_DIR))):
        runFile(file, True)

def checkResults():
    pass

def runFile(fileName, verbose):
    process = subprocess.Popen(['./team.native', fileName], 
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    
    if verbose:
        if not os.path.exists('log'):
            os.makedirs('log')
        with open('log/{}.log'.format(fileName.split('/')[-1].split('.')[0]), 'w+') as fo:
            toWrite = stdout if stdout else stderr
            fo.write(toWrite.decode('utf-8'))
                

def interpretCommand(command):
    return True if command[0].upper() == "T" else False

def compile(topLevel):
    process = subprocess.Popen(['ocamlbuild', topLevel], 
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    if 'failed' in stdout.decode('utf-8'):
        print("Error detected when compiling {}. See message below.".format(topLevel))
        print(stdout.decode('utf-8'))
        sys.exit()

def clean(target):
    if target == "log":
        os.system("rm -r log")
    elif target == "ocaml":
        process = subprocess.Popen(['ocamlbuild', '-clean'], 
                                stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE)

if __name__ == "__main__":
    parser = optparse.OptionParser()
    parser.add_option('-v', '--verbose', 
                      dest='verbose', default='True', 
                      help='True to write AST.')
    parser.add_option('-r', '--recompile', 
                      dest='recompile', default='False', 
                      help='True to recompile top level.')
    parser.add_option('-t', '--testFile', 
                      dest='testFile', default='', 
                      help='Specify one test file. AST is written.')
    parser.add_option('-l', '--topLevel', 
                      dest='topLevel', default='team.native', 
                      help='Name of the top level')

    options, args = parser.parse_args()
    verbose = interpretCommand(options.verbose)
    recompile = interpretCommand(options.recompile)
    testFile = options.testFile
    topLevel = options.topLevel

    if recompile or topLevel not in os.listdir('/'):
        compile(topLevel)
    
    if testFile != '':
        runFile(testFile, True)
    else:
        runValidTests()
        runInValidTests()
        checkResults()
        print("\n!!!NOTE!!!")
        print("\t1. checkResults is not implemented yet")
        print("\t2. AST are printed to log/fileName.log")
        print("\n!!!Error Found So Far!!!")
        print("\t1. The order of evaluation is reversed.")
        if not verbose and os.path.exists('log'):
            clean('log')

    clean('ocaml')