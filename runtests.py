#!/usr/bin/env python3
import sys
import os
import subprocess
import optparse
import shutil

class bcolors:
    OKGREEN = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    WARNING = '\033[93m'
    UNDERLINE = '\033[4m'

VALID_FILE_DIR = 'scanner_parser_tests/valid'
INVALID_FILE_DIR = 'scanner_parser_tests/invalid'


def runValidTests():
    print("\nRunning Valid Tests...\n")
    for file in map(lambda x: "/".join((VALID_FILE_DIR, x)), filter(lambda x: "tm" in x, os.listdir(VALID_FILE_DIR))):
        logFilename = runFile(file, True)
        refFilename = "scanner_parser_ref/{}".format(logFilename.split("/")[-1])
        checkResults(logFilename, refFilename)

def runInValidTests():
    print("\nRunning Invalid Tests...\n")
    for file in map(lambda x: "/".join((INVALID_FILE_DIR, x)), filter(lambda x: "tm" in x, os.listdir(INVALID_FILE_DIR))):
        logFilename = runFile(file, True)
        refFilename = "scanner_parser_ref/{}".format(logFilename.split("/")[-1])
        checkResults(logFilename, refFilename)

def printFailedTestMessage(logFile):
    tmFile = logFile.split("/")[-1].split(".")[0]
    print(bcolors.FAIL +"{:20s} -- FAILED!\n".format(tmFile + ".tm") + bcolors.ENDC)

def printSuccessTestMessage(logFile):
    tmFile = logFile.split("/")[-1].split(".")[0]
    print(bcolors.OKGREEN +"{:20s} -- OK!\n".format(tmFile + ".tm") + bcolors.ENDC)

def checkResults(f_astGenerated, f_astReference):
    astGenerated = [line for line in open(f_astGenerated)]
    astReference = [line for line in open(f_astReference)]

    if len(astGenerated) != len(astReference):
        printFailedTestMessage(f_astGenerated)
        return 
    for index, astGenerateLine in enumerate(astGenerated):
        if astGenerateLine != astReference[index]:
            printFailedTestMessage(f_astGenerated)
            return
    printSuccessTestMessage(f_astGenerated)

def runFile(fileName, verbose):
    process = subprocess.Popen(['./team.native', fileName], 
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    
    if verbose:
        if not os.path.exists('log'):
            os.makedirs('log')
        filename = 'log/{}.log'.format(fileName.split('/')[-1].split('.')[0])
        with open(filename, 'w+') as fo:
            toWrite = stdout if stdout else stderr
            fo.write(toWrite.decode('utf-8'))
    return filename
                

def interpretCommand(command):
    return True if command[0].upper() == "T" else False

def compile(topLevel):
    process = subprocess.Popen(['ocamlbuild', topLevel], 
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    if 'failed' in stdout.decode('utf-8') or "Error" in stdout.decode('utf-8'):
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

def moveLog():
    if not os.path.exists("log/valid"):
        os.makedirs("log/valid")
    if not os.path.exists("log/invalid"):
        os.makedirs("log/invalid")
    for file in [f for f in os.listdir("log") if "log" in f]:
        if "bad" not in file:
            shutil.move("log/{}".format(file), "log/valid/{}".format(file))
        else:
            shutil.move("log/{}".format(file), "log/invalid/{}".format(file))


if __name__ == "__main__":
    parser = optparse.OptionParser()
    parser.add_option('-v', '--verbose', 
                      dest='verbose', default='True', 
                      help='True to write AST.')
    parser.add_option('-c', '--recompile', 
                      dest='recompile', default='False', 
                      help='True to recompile top level.')
    parser.add_option('-t', '--testFile', 
                      dest='testFile', default='', 
                      help='Specify one test file. AST is written.')
    parser.add_option('-r', '--reference',
                      dest='reference', default='',
                      help='The reference to compare generated AST.')
    parser.add_option('-l', '--topLevel', 
                      dest='topLevel', default='team.native', 
                      help='Name of the top level')

    options, args = parser.parse_args()
    verbose = interpretCommand(options.verbose)
    recompile = interpretCommand(options.recompile)
    testFile = options.testFile
    topLevel = options.topLevel
    reference = options.reference

    if reference != '' and testFile == '':
        key = input("Error detected!\nReference specified with no test file specified.\nExiting...\n")
        sys.exit()
        
    if recompile or topLevel not in os.listdir('/'):
        compile(topLevel)
    
    if testFile != '':
        runFile(testFile, True)
        if reference != "":
            checkResults("log/{}.log".format(testFile.split("/")[-1].split(".")[0]), reference)
    else:
        runValidTests()
        runInValidTests()
        print(bcolors.WARNING + "\n!!!NOTE!!!" + bcolors.ENDC)
        print("\t1. AST are printed to log/(in)valid/fileName.log")
        print(bcolors.WARNING + "\n!!!Error Found So Far!!!" + bcolors.ENDC)

        print("\t None :D")
        moveLog()
        if not verbose and os.path.exists('log'):
            clean('log')
        

    clean('ocaml')