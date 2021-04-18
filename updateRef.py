import os
from os import listdir
from os.path import isfile, join

phase = ["ast", "sast", "codegen"]
mode = ["a", "s", "l"]
for i, m in enumerate(phase):

    testdir = "./{}_tests/".format(m)

    onlyfiles = [f for f in listdir(testdir) if isfile(join(testdir, f))]

    os.system("make")
    if m == "codegen":
        for file in onlyfiles:
            filename = file.split(".")[0]
            if file.startswith("bad"):
                os.system("./team.native -{} {}_tests/{}.tm | lli 2> {}_ref/{}.log".format(mode[i], m, filename, m, filename))
            else:  
                os.system("./team.native -{} {}_tests/{}.tm| lli > {}_ref/{}.log".format(mode[i], m, filename, m, filename))
    else:
        for file in onlyfiles:
            filename = file.split(".")[0]
            if file.startswith("bad"):
                os.system("./team.native -{} {}_tests/{}.tm 2> {}_ref/{}.log".format(mode[i], m, filename, m, filename))
            else:  
                os.system("./team.native -{} {}_tests/{}.tm > {}_ref/{}.log".format(mode[i], m, filename, m, filename))