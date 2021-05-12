import os
from os import listdir
from os.path import isfile, join

phase = ["ast", "sast", "codegen"]
mode = ["a", "r", "l"]
os.system("make clean")
os.system("make")
for i, m in enumerate(phase):

    testdir = "./{}_tests/".format(m)

    onlyfiles = [f for f in listdir(testdir) if isfile(join(testdir, f))]
    if m == "codegen":
        for file in onlyfiles:
            filename = file.split(".")[0]
            if file.startswith("bad"):
                os.system("./compile.sh tests/{}_tests/{}.tm run 2> {}_ref/{}.log".format(m, filename, m, filename))
            else:  
                os.system("./compile.sh tests/{}_tests/{}.tm run > {}_ref/{}.log".format(m, filename, m, filename))
            os.system("./compile.sh tests/{}_tests/{}.tm clean".format(m, filename))
    else:
        for file in onlyfiles:
            filename = file.split(".")[0]
            if file.startswith("bad"):
                os.system("./team.native -{} tests/{}_tests/{}.tm 2> {}_ref/{}.log".format(mode[i], m, filename, m, filename))
            else:  
                os.system("./team.native -{} tests/{}_tests/{}.tm > {}_ref/{}.log".format(mode[i], m, filename, m, filename))