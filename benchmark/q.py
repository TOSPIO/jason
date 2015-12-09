import sys
import json

if __name__ == '__main__':
  times = int(sys.argv[1])
  f = open('../fixtures/sample1.json', 'r').read()
  for i in range(times):
    p = json.loads(f)
