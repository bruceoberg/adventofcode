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
aaCHit = [[0 for x in range(w)] for y in range(h)]

for str in aStrLine:
	id, xMin, yMin, dX, dY = [int(g) for g in pat.match(str).groups()]
	
	xMax = xMin + dX
	yMax = yMin + dY
	
	for x in xrange(xMin, xMax):
		
		for y in xrange(yMin, yMax):
		
			aaCHit[x][y] += 1
	
if False:		
	for x in range(10):
		for y in range(10):
			print aaCHit[x][y],
		print
		
aaCMulti = [[1 if cHit > 1 else 0 for cHit in aCHit] for aCHit in aaCHit]

cMulti = sum(sum(aaCMulti, []))

print cMulti

	
