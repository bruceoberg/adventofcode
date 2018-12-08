import os
import re
import sys

pathThis = os.path.realpath(__file__)
pathBase, ext = os.path.splitext(pathThis)
pathInput = pathBase + '.txt'

aStrLine = [strLine.strip() for strLine in open(pathInput, 'rt').xreadlines()]

if False:
    for strLine in aStrLine:
        print 'IN> ', line,

pat = re.compile('^#(\d+) @ ([\d.]+),([\d.]+): ([\d.]+)x([\d.]+)$')

w, h = 1000, 1000
aaSetId = [[set() for x in range(w)] for y in range(h)]
mpIdSetXY = {}

for str in aStrLine:
	id, xMin, yMin, dX, dY = [int(g) for g in pat.match(str).groups()]
	
	xMax = xMin + dX
	yMax = yMin + dY
	
	for x in xrange(xMin, xMax):
		
		for y in xrange(yMin, yMax):
		
			aaSetId[x][y].add(id)
			mpIdSetXY.setdefault(id, set()).add((x,y))
	
for id, setXY in mpIdSetXY.iteritems():
	if all([len(aaSetId[x][y]) == 1 for x, y in setXY]):
		print id
