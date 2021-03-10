import os

test_suite = os.listdir('tests')
for test_file in test_suite:
  res = os.system('./team.native tests/{} 2>/dev/null >/dev/null'.format(test_file))
  if res == 0:
    print("{}: Passed!".format(test_file))
  else:
    print("{}: Error! Error code {}".format(test_file, res))