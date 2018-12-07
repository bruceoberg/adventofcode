import os

pathThis = os.path.realpath(__file__)
pathBase, ext = os.path.splitext(pathThis)
pathInput = pathBase + '.txt'

aStrLine = [strLine for strLine in open(pathInput, 'rt').xreadlines()]

if False:
    for strLine in aStrLine:
        print 'IN> ', line,

freq = 0
setFreqSeen = set([freq])

fKeepGoing = True
while fKeepGoing:
    for iStrLine in range(len(aStrLine)):

        strLine = aStrLine[iStrLine]
        
        dFreq = int(strLine)

        freq += dFreq

        if freq in setFreqSeen:
            fKeepGoing = False
            break

        setFreqSeen.add(freq)

print freq
