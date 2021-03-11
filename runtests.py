import os
import subprocess

# Color printing purposes
class bcolors:
    PASS = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

test_suite = os.listdir('tests')
neg_tests = []
pos_tests = []

# Source: https://code-maven.com/python-capture-stdout-stderr-exit
def run(command):
  proc = subprocess.Popen(command,
    stdout = subprocess.PIPE,
    stderr = subprocess.PIPE,
  )
  stdout, stderr = proc.communicate()

  return proc.returncode, stdout, stderr

# Organize tests: Pos tests then neg tests
for test_file in test_suite:
  if test_file.startswith('bad'):
    neg_tests.append(test_file)
  else:
    pos_tests.append(test_file)

test_suite = pos_tests + neg_tests

for test_file in test_suite:
  code, out, err = run(['./team.native', 'tests/{}'.format(test_file)])
  if code == 0:
    print(bcolors.PASS + "{:25}==> Passed!".format(test_file) + bcolors.ENDC)
  else:
    print(bcolors.FAIL + "{:25}==> {}".format(test_file, err.decode('utf-8').rstrip()) + bcolors.ENDC)
