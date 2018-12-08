import os

pathThis = os.path.realpath(__file__)
pathBase, ext = os.path.splitext(pathThis)
pathInput = pathBase + '.txt'

aStrLine = [strLine.strip() for strLine in open(pathInput, 'rt').xreadlines()]

if False:
    for strLine in aStrLine:
        print 'IN> ', line,

cMatch2 = 0
cMatch3 = 0

for strLine in aStrLine:

    mpChCch = dict([(ch, 0) for ch in 'abcdefghijklmnopqrstuvwxyz'])

    for ch in strLine:
        mpChCch[ch] += 1

    setCch = set([cCh for ch, cCh in mpChCch.iteritems()])

    if 2 in setCch:
        cMatch2 += 1

    if 3 in setCch:
        cMatch3 += 1

print cMatch2 * cMatch3
