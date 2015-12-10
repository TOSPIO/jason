import sys
import json

if __name__ == '__main__':
  times = int(sys.argv[1])
  f = open('../fixtures/citylots.json', 'r').read()
  for i in range(times):
    p = json.loads(f)
