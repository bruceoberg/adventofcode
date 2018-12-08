import os
import sys

pathThis = os.path.realpath(__file__)
pathBase, ext = os.path.splitext(pathThis)
pathInput = pathBase + '.txt'

aStrLine = [strLine.strip() for strLine in open(pathInput, 'rt').xreadlines()]

if False:
    for strLine in aStrLine:
        print 'IN> ', line,

setStr = set(aStrLine)

while setStr:
	
	strA = setStr.pop()
	
	for strB in setStr:
		
		if len(strA) != len(strB):
			continue
			
		aCEq = [1 if strA[i] != strB[i] else 0 for i in range(len(strA))]
		
		#print aCEq
		
		if sum(aCEq) != 1:
			continue
			
		iC1 = aCEq.index(1)
		assert(iC1 != -1)
		
		print strA[:iC1] + strA[iC1+1:]
		
		sys.exit()
		
print '(none)'
