#!/usr/bin/env python3
import sys
import os
import subprocess
import optparse

class bcolors:
    OKGREEN = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    WARNING = '\033[93m'
    UNDERLINE = '\033[4m'

SCANNER_PARSER_DIR = ("tests/ast_tests", "ref/ast_ref")
SEMANT_DIR = ("tests/sast_tests", "ref/sast_ref")
CODEGEN_DIR = ("tests/codegen_tests", "ref/codegen_ref")
EXTENDED_DIR = ("tests/extended_tests", "ref/extended_ref")

def getCompiledFiles():
    fileList = [f for f in os.listdir(".") if ".o" in f]
    return " ".join(fileList)

def sortingKey(fileName):
    return fileName.split(".")[0]

def runTests(testMode):
    if testMode == "sast":
        dirTuple = SEMANT_DIR
    elif testMode == "ast":
        dirTuple = SCANNER_PARSER_DIR
    elif testMode == "codegen":
        dirTuple = CODEGEN_DIR
    elif testMode == "extended":
        dirTuple = EXTENDED_DIR
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

def checkResults(f_generated, f_reference):
    generated = [line for line in open(f_generated)]
    reference = [line for line in open(f_reference)]

    if len(generated) != len(reference):
        printFailedTestMessage(f_generated)
        return
    for index, astGenerateLine in enumerate(generated):
        if astGenerateLine != reference[index]:
            printFailedTestMessage(f_generated)
            print("+" * 100)
            print(bcolors.WARNING + bcolors.UNDERLINE + "Running diff...\n" + bcolors.ENDC)
            process = subprocess.Popen(["diff", "-y", f_generated, f_reference],
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
            stdout, stderr = process.communicate()
            print(bcolors.WARNING +
                  "command: diff -y {} (output) {} (standard)\n".format(f_generated, f_reference) +
                  bcolors.ENDC)
            print(bcolors.WARNING + stdout.decode("utf-8") + bcolors.ENDC)
            print("+" * 100)
            return
    printSuccessTestMessage(f_generated)

def runFile(fileName, testMode, userInput=False):
    if testMode == "ast":
        flag = "-a"
    elif testMode == "sast":
        flag = "-s"
    elif testMode == "codegen":
        flag = "-l"
    elif testMode == "extended":
        flag = "-l"
    else:
        print(bcolors.FAIL + "Test mode: {} not supported".format(testMode) + bcolors.ENDC)
        sys.exit()

    if testMode not in ["codegen", "extended"] or "bad" in fileName:
        command = ['./team.native', flag, fileName]
    else:
        command = ["./scripts/compile.sh", fileName, "run"]
    process = subprocess.Popen(command,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    clean = ["./scripts/compile.sh", fileName, "clean"]
    process = subprocess.Popen(clean,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)

    if userInput:
        dir_prefix = "user_log"
    else:
        dir_prefix = "log/" + "_".join((testMode, "log"))

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
    parser.add_option('-m', '--testMode',
                      dest='testMode', default='extended',
                      help='ast, sast, codegen, extended, all')

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
        testMode = [testMode] if testMode != "all" else ["ast", "sast", "codegen", "extended"]
        for m in testMode:
            print(bcolors.WARNING + bcolors.UNDERLINE + "\nTest mode: {}\n".format(m) + bcolors.ENDC)
            runTests(m)
