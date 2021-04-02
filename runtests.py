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

SCANNER_PARSER_DIR = ("ast_tests", "ast_ref")
SEMANT_DIR = ("sast_tests", "sast_ref")
CODEGEN_DIR = ("codegen_tests", "codegen_ref")

def sortingKey(fileName):
    return fileName.split(".")[0]

def runTests(testMode):
    if testMode == "sast":
        dirTuple = SEMANT_DIR
    elif testMode == "ast":
        dirTuple = SCANNER_PARSER_DIR
    elif testMode == "codegen":
        dirTuple = CODEGEN_DIR
    else:
        print(bcolors.FAIL + "Test mode: {} not supported".format(testMode) + bcolors.ENDC)
        sys.exit()

    testFileDir, refFileDir = dirTuple
    testFiles = list(map(lambda x: "/".join((testFileDir, x)), sorted(filter(lambda x: "tm" in x, os.listdir(testFileDir)), key=sortingKey)))
    refFiles = list(map(lambda x: "/".join((refFileDir, x)), sorted(filter(lambda x: "log" in x, os.listdir(refFileDir)), key=sortingKey)))

    assert len(testFiles) == len(refFiles)

    for i in range(len(testFiles)):
        testFile, refFile = testFiles[i], refFiles[i]

        logFile = runFile(testFile, testMode)
        checkResults(logFile, refFile)

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

def runFile(fileName, testMode, userInput=False):
    if testMode == "ast":
        flag = "-a"
    elif testMode == "sast":
        flag = "-s"
    elif testMode == "codegen":
        flag = "-l"
    else:
        print(bcolors.FAIL + "Test mode: {} not supported".format(testMode) + bcolors.ENDC)
        sys.exit()
    
    command = ['./team.native', flag, fileName]

    process = subprocess.Popen(command, 
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE)
    
    if testMode == "codegen":
        process = subprocess.Popen(["lli"],
                                   stdin=process.stdout,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)

    stdout, stderr = process.communicate()
    
    if userInput:
        dir_prefix = "user_log"
    else:
        dir_prefix = "_".join((testMode, "log"))

    if not os.path.exists(dir_prefix):
        os.makedirs(dir_prefix)
    filename = '{}/{}.log'.format(dir_prefix, fileName.split('/')[-1].split('.')[0])
    with open(filename, 'w+') as fo:
        toWrite = stdout if stdout else stderr
        fo.write(toWrite.decode('utf-8'))
    return filename
                

def interpretCommand(command):
    return True if command[0].upper() == "T" else False

def compile(topLevel):
    process = subprocess.Popen(['ocamlbuild', '-use-ocamlfind', topLevel], 
                               stdout=subprocess.PIPE, 
                               stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    if 'failed' in stdout.decode('utf-8') or "Error" in stdout.decode('utf-8'):
        print(bcolors.FAIL + "Error detected when compiling {}. See message below.".format(topLevel) + bcolors.ENDC)
        print(stdout.decode('utf-8'))
        sys.exit()

def clean(target):
    if target == "ocaml":
        process = subprocess.Popen(['ocamlbuild', '-clean'], 
                                stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE)

if __name__ == "__main__":
    parser = optparse.OptionParser()
    parser.add_option('-c', '--recompile', 
                      dest='recompile', default='True', 
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
    parser.add_option('-m', '--testMode', 
                      dest='testMode', default='ast',
                      help='ast, sast, codegen, all')

    options, args = parser.parse_args()
    recompile = interpretCommand(options.recompile)
    testFile = options.testFile
    topLevel = options.topLevel
    reference = options.reference
    testMode = options.testMode

    if reference != '' and testFile == '':
        print("Error detected!\nReference specified with no test file specified.\nExiting...\n")
        sys.exit()
        
    if recompile or topLevel not in os.listdir('/'):
        compile(topLevel)
    
    if testFile != '':
        if testMode == "all":
            print("Error detected!\n Test mode cannot be all when testing a single file")
            sys.exit()
        logFile = runFile(testFile, testMode, True)
        checkResults(logFile, reference)
    else:
        testMode = [testMode] if testMode != "all" else ["ast", "sast", "codegen"]
        for m in testMode:
            print(bcolors.WARNING + bcolors.UNDERLINE + "\nTest mode: {}\n".format(m) + bcolors.ENDC)
            runTests(m)

    clean('ocaml')