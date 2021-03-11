import os
import subprocess
import sys

test_suite = os.listdir('tests')

# Source: https://code-maven.com/python-capture-stdout-stderr-exit
def run(command):
  proc = subprocess.Popen(command,
    stdout = subprocess.PIPE,
    stderr = subprocess.PIPE,
  )
  stdout, stderr = proc.communicate()

  return proc.returncode, stdout, stderr

for test_file in test_suite:
  code, out, err = run(['./team.native', 'tests/{}'.format(test_file)])

  if code == 0:
    print("{}: Passed!".format(test_file))
  else:
    print("{}: {}".format(test_file, err.decode('utf-8').rstrip()))