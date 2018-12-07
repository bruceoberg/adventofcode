import os
import sys

pathThis = sys.argv[0]
pathBase, ext = os.path.splitext(pathThis)
pathInput = pathBase + '.txt'

for line in open(pathInput, 'rt').xreadlines():
    print 'IN> ', line,