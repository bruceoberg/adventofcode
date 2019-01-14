import re
from collections import namedtuple

aDayInDayFn = (
#	(0, '000'),
	(6, '065'),
)

SLoc = namedtuple('SLoc', ['x', 'y'])

def Day060(aStrIn):
	pat = re.compile('^(\d+), (\d+)$')
	
	aLoc = []
	
	mpLocCh = {} if len(aStrIn) <= 26 else None
	
	for strIn in aStrIn:
		x, y = [int(s) for s in pat.match(strIn).groups()]
		
		aLoc.append(SLoc(x, y))
		
		if mpLocCh is not None:
			mpLocCh[aLoc[-1]] = chr(ord('a') + len(aLoc) - 1)
		
	locMin = aLoc[0]
	locMax = aLoc[0]
	
	for loc in aLoc:
		locMin = SLoc(min(locMin.x, loc.x), min(locMin.y, loc.y))
		locMax = SLoc(max(locMax.x, loc.x), max(locMax.y, loc.y))
		
	# expand one cell around limits
		
	locMin = SLoc(locMin.x - 1, locMin.y - 1)
	locMax = SLoc(locMax.x + 1, locMax.y + 1)
	
	aX = range(locMin.x, locMax.x + 1)
	aY = range(locMin.y, locMax.y + 1)
	
	print 'len:', len(aLoc)		
	print 'min:', locMin
	print 'max:', locMax
	
	mpXyLocD = {}
	
	for x in aX:
		if mpLocCh is None:
			print 'collect:', x
		for y in aY:
			mpXyLocD[(x, y)]= {}
			mpLocD = mpXyLocD[(x, y)]
			for loc in aLoc:
				mpLocD[loc] = abs(x - loc.x) + abs(y - loc.y)
				
	mpLocCHit = {}
	setLocInf = set()
	mpXyLocBest = {}
	
	for x in aX:
		if mpLocCh is None:
			print 'compare:', x
		for y in aY:
			mpLocD = mpXyLocD[(x, y)]
			aDLoc = sorted([(d, loc) for loc, d in mpLocD.iteritems()])
			if aDLoc[0][0] == aDLoc[1][0]:
				continue
			d, loc = aDLoc[0]
			mpXyLocBest[(x,y)] = loc
			#print ' found:', d, loc
			mpLocCHit.setdefault(loc, 0)
			mpLocCHit[loc] += 1
			
			if x == locMin.x or x == locMax.x or y == locMin.y or y == locMax.y:
				setLocInf.add(loc)

	if mpLocCh is not None:
		for y in aY:
			for x in aX:
				try:
					locBest = mpXyLocBest[(x, y)]
					ch = mpLocCh[locBest]
					if locBest == SLoc(x, y):
						ch = ch.upper()
				except KeyError:
					ch = '.'
					
				print ch,
				
			print
					

	setLocFin = set(mpLocCHit.keys()) - setLocInf
	
	aCHitLoc = sorted([(mpLocCHit[loc], loc) for loc in setLocFin])
	
	cHitMost, locMost = aCHitLoc[-1]
	
	print 'cHitMost:', cHitMost
	print 'locMost:', locMost

	if mpLocCh is not None:	
		print 'chMost:', mpLocCh[locMost]
			
def Day065(aStrIn):
	pat = re.compile('^(\d+), (\d+)$')
	
	aLoc = []
	
	mpLocCh = {} if len(aStrIn) <= 26 else None
	
	for strIn in aStrIn:
		x, y = [int(s) for s in pat.match(strIn).groups()]
		
		aLoc.append(SLoc(x, y))
		
		if mpLocCh is not None:
			mpLocCh[aLoc[-1]] = chr(ord('a') + len(aLoc) - 1)
			
	setLoc = set(aLoc)
		
	locMin = aLoc[0]
	locMax = aLoc[0]
	
	for loc in aLoc:
		locMin = SLoc(min(locMin.x, loc.x), min(locMin.y, loc.y))
		locMax = SLoc(max(locMax.x, loc.x), max(locMax.y, loc.y))
		
	# expand one cell around limits
		
	locMin = SLoc(locMin.x - 1, locMin.y - 1)
	locMax = SLoc(locMax.x + 1, locMax.y + 1)
	
	aX = range(locMin.x, locMax.x + 1)
	aY = range(locMin.y, locMax.y + 1)
	
	print 'len:', len(aLoc)		
	print 'min:', locMin
	print 'max:', locMax
	
	mpXyLocD = {}
	mpXyDTot = {}
	
	for x in aX:
		if mpLocCh is None:
			print 'collect:', x
		for y in aY:
			mpXyLocD[(x, y)]= {}
			mpLocD = mpXyLocD[(x, y)]
			mpXyDTot[(x, y)] = 0
			for loc in aLoc:
				d = abs(x - loc.x) + abs(y - loc.y)
				mpLocD[loc] = d
				mpXyDTot[(x, y)] += d
	
	if mpLocCh is not None:
		for y in aY:
			for x in aX:
				try:
					ch = mpLocCh[SLoc(x, y)].upper()
				except KeyError:
					dTot = mpXyDTot[(x, y)]
					ch = '#' if dTot < 32 else '.'
					
				print ch,
				
			print
			
	cXyOk = 0
	
	for y in aY:
		for x in aX:
			if mpXyDTot[(x, y)] < 10000:
				cXyOk += 1
				
	print 'cXyOk:', cXyOk
	
def Day070(aStrIn):
	pass

def Day075(aStrIn):
	pass

def Day080(aStrIn):
	pass

def Day085(aStrIn):
	pass

def Day090(aStrIn):
	pass

def Day095(aStrIn):
	pass

def Day110(aStrIn):
	pass

def Day115(aStrIn):
	pass

def Day120(aStrIn):
	pass

def Day125(aStrIn):
	pass

def Day130(aStrIn):
	pass

def Day135(aStrIn):
	pass

def Day140(aStrIn):
	pass

def Day145(aStrIn):
	pass

def Day150(aStrIn):
	pass

def Day155(aStrIn):
	pass

def Day160(aStrIn):
	pass

def Day165(aStrIn):
	pass

def Day170(aStrIn):
	pass

def Day175(aStrIn):
	pass

def Day180(aStrIn):
	pass

def Day185(aStrIn):
	pass

def Day190(aStrIn):
	pass

def Day195(aStrIn):
	pass

def Day200(aStrIn):
	pass

def Day205(aStrIn):
	pass

def Day210(aStrIn):
	pass

def Day215(aStrIn):
	pass

def Day220(aStrIn):
	pass

def Day225(aStrIn):
	pass

def Day230(aStrIn):
	pass

def Day235(aStrIn):
	pass

def Day240(aStrIn):
	pass

def Day245(aStrIn):
	pass

def Day250(aStrIn):
	pass

def Day255(aStrIn):
	pass

def Day000(aStrIn):
	for strIn in aStrIn:
		print 'IN>> ', strIn
		
def Day010(aStrIn):
	freq = 0
	
	for strIn in aStrIn:
	
	    freq += int(strIn)
	
	print freq
	
def Day015(aStrIn):
	freq = 0
	setFreqSeen = set([freq])
	
	fKeepGoing = True
	while fKeepGoing:
	    for strIn in aStrIn:
	
	        dFreq = int(strIn)
	
	        freq += dFreq
	
	        if freq in setFreqSeen:
	            fKeepGoing = False
	            break
	
	        setFreqSeen.add(freq)
	
	print freq

def Day020(aStrIn):
	cMatch2 = 0
	cMatch3 = 0
	
	for strIn in aStrIn:
	
	    mpChCch = dict([(ch, 0) for ch in 'abcdefghijklmnopqrstuvwxyz'])
	
	    for ch in strIn:
	        mpChCch[ch] += 1
	
	    setCch = set([cCh for ch, cCh in mpChCch.iteritems()])
	
	    if 2 in setCch:
	        cMatch2 += 1
	
	    if 3 in setCch:
	        cMatch3 += 1
	
	print cMatch2 * cMatch3

def Day025(aStrIn):

	setStr = set(aStrIn)
	
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
			assert iC1 != -1
			
			print strA[:iC1] + strA[iC1+1:]
			
			return
			
	print '(none)'

def Day030(aStrIn):
	
	pat = re.compile('^#(\d+) @ ([\d.]+),([\d.]+): ([\d.]+)x([\d.]+)$')
	
	w, h = 1000, 1000
	aaCHit = [[0 for x in range(w)] for y in range(h)]
	
	for strIn in aStrIn:
		id, xMin, yMin, dX, dY = [int(g) for g in pat.match(strIn).groups()]
		
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

def Day035(aStrIn):
	
	pat = re.compile('^#(\d+) @ ([\d.]+),([\d.]+): ([\d.]+)x([\d.]+)$')
	
	w, h = 1000, 1000
	aaSetId = [[set() for x in range(w)] for y in range(h)]
	mpIdSetXY = {}
	
	for strIn in aStrIn:
		id, xMin, yMin, dX, dY = [int(g) for g in pat.match(strIn).groups()]
		
		xMax = xMin + dX
		yMax = yMin + dY
		
		for x in xrange(xMin, xMax):
			
			for y in xrange(yMin, yMax):
			
				aaSetId[x][y].add(id)
				mpIdSetXY.setdefault(id, set()).add((x,y))
		
	for id, setXY in mpIdSetXY.iteritems():
		if all([len(aaSetId[x][y]) == 1 for x, y in setXY]):
			print id

def Day040(aStrIn):
	# [1518-11-03 00:05] Guard #10 begins shift
	# [1518-11-03 00:24] falls asleep
	# [1518-11-03 00:29] wakes up
	
	pat = re.compile('^\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.....) (.*)$')

	
	aRec = []
	
	for strIn in aStrIn:
		aG = pat.match(strIn).groups()
		
		year, month, day, hour, minute = [int(g) for g in aG[:5]]
		
		word = aG[5]
		
		if word == 'Guard':
			verb = 'a'
			guard = int(aG[6][1:].split()[0])
		elif word == 'falls':
			verb = 'b'
			guard = -1
		elif word == 'wakes':
			verb = 'c'
			guard = -1
		else:
			print 'bad line:', strIn
			raise Exception
			
		aRec.append((year, month, day, hour, minute, verb, guard))
		
	guardCur = -1
	minuteSleep = -1
	
	mpGuardCSleep = {}
	mpGuardMinuteCSleep = {}
	
	cSleepGuardMost = -1
	guardMost = -1
	
	for (year, month, day, hour, minute, verb, guard) in sorted(aRec):
		if verb == 'a':
			guardCur = guard
			minuteSleep = -1
		elif verb == 'b':
			assert guardCur != -1
			assert minuteSleep == -1
			minuteSleep = minute
		else:
			assert verb == 'c'
			assert guardCur != -1
			assert minuteSleep != -1
			
			setMinuteSleep = set(range(minuteSleep, minute))
			
			mpGuardCSleep.setdefault(guardCur, 0)
			mpGuardCSleep[guardCur] += len(setMinuteSleep)
			
			if mpGuardCSleep[guardCur] > cSleepGuardMost:
				cSleepGuardMost = mpGuardCSleep[guardCur]
				guardMost = guardCur
			
			for minuteRange in setMinuteSleep:
				mpGuardMinuteCSleep.setdefault(guardCur, {}).setdefault(minuteRange, 0)
				mpGuardMinuteCSleep[guardCur][minuteRange] += 1
				
			minuteSleep = -1
				
	cSleepMinuteMost, minuteMost = sorted([(cSleep, minute) for minute, cSleep in mpGuardMinuteCSleep[guardMost].iteritems()])[-1]
	
	print 'guardMost:', guardMost
	print 'cSleepGuardMost:', cSleepGuardMost
	print 'minuteMost:', minuteMost
	print 'cSleepMinuteMost:', cSleepMinuteMost
	
	print 'answer:', guardMost * minuteMost
		
def Day045(aStrIn):
	# [1518-11-03 00:05] Guard #10 begins shift
	# [1518-11-03 00:24] falls asleep
	# [1518-11-03 00:29] wakes up
	
	pat = re.compile('^\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.....) (.*)$')

	
	aRec = []
	
	for strIn in aStrIn:
		aG = pat.match(strIn).groups()
		
		year, month, day, hour, minute = [int(g) for g in aG[:5]]
		
		word = aG[5]
		
		if word == 'Guard':
			verb = 'a'
			guard = int(aG[6][1:].split()[0])
		elif word == 'falls':
			verb = 'b'
			guard = -1
		elif word == 'wakes':
			verb = 'c'
			guard = -1
		else:
			print 'bad line:', strIn
			raise Exception
			
		aRec.append((year, month, day, hour, minute, verb, guard))
		
	guardCur = -1
	minuteSleep = -1
	
	mpGuardMinuteCSleep = {}
	
	for (year, month, day, hour, minute, verb, guard) in sorted(aRec):
		if verb == 'a':
			guardCur = guard
			minuteSleep = -1
		elif verb == 'b':
			assert guardCur != -1
			assert minuteSleep == -1
			minuteSleep = minute
		else:
			assert verb == 'c'
			assert guardCur != -1
			assert minuteSleep != -1
			
			setMinuteSleep = set(range(minuteSleep, minute))
			
			for minuteRange in setMinuteSleep:
				mpGuardMinuteCSleep.setdefault(guardCur, {}).setdefault(minuteRange, 0)
				mpGuardMinuteCSleep[guardCur][minuteRange] += 1
				
			minuteSleep = -1
				
	aSmg = []
	for guard, mpMinuteCSleep in mpGuardMinuteCSleep.iteritems():
		for minute, cSleep in mpMinuteCSleep.iteritems():
			aSmg.append((cSleep, minute, guard))
			
	cSleepMost, minuteMost, guardMost = sorted(aSmg)[-1]
	
	print 'guardMost:', guardMost
	print 'minuteMost:', minuteMost
	print 'cSleepMost:', cSleepMost
	
	print 'answer:', guardMost * minuteMost

def CChDay5Eater(strIn):
	setChChEat = \
		set(zip('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz',
				'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'))
				
	cChProgMod = 1000
	cChProg = len(strIn) - (len(strIn) % cChProgMod)
	
	iCh = 0
	while iCh < len(strIn) - 1:
		chch = (strIn[iCh], strIn[iCh+1])
		if chch in setChChEat:
			strIn = strIn[:iCh] + strIn[iCh+2:]
			#if len(strIn) < cChProg:
			#print 'ate', ''.join(chch), 'at pos', iCh, 'len now', len(strIn), 'str now', strIn[:10], '...', strIn[iCh-5:iCh+5]
			#	cChProg = len(strIn) - (len(strIn) % cChProgMod)				
			if iCh > 0:
				iCh -= 1
		else:
			iCh += 1

	return len(strIn)

def Day050(aStrIn):
	print CChDay5Eater(aStrIn[0])
	
def Day055(aStrIn):
	
	cChBest = len(aStrIn[0])
	
	for dCh in range (0,26):
		chLo = chr(ord('a') + dCh)
		chUp = chr(ord('A') + dCh)
		
		strAll = aStrIn[0].replace(chLo, '')
		strAll = strAll.replace(chUp, '')

		cCh = CChDay5Eater(strAll)
		
		print chLo, cCh
		
		if cCh < cChBest:
			cChBest = cCh
	
	print cChBest
		
mpDayFn = {
	'000': Day000,
	'010': Day010,
	'015': Day015,
	'020': Day020,
	'025': Day025,
	'030': Day030,
	'035': Day035,
	'040': Day040,
	'045': Day045,
	'050': Day050,
	'055': Day055,
	'060': Day060,
	'065': Day065,
	'070': Day070,
	'075': Day075,
	'080': Day080,
	'085': Day085,
	'090': Day090,
	'095': Day095,
	'110': Day110,
	'115': Day115,
	'120': Day120,
	'125': Day125,
	'130': Day130,
	'135': Day135,
	'140': Day140,
	'145': Day145,
	'150': Day150,
	'155': Day155,
	'160': Day160,
	'165': Day165,
	'170': Day170,
	'175': Day175,
	'180': Day180,
	'185': Day185,
	'190': Day190,
	'195': Day195,
	'210': Day210,
	'215': Day215,
	'220': Day220,
	'225': Day225,
	'230': Day230,
	'235': Day235,
	'240': Day240,
	'245': Day245,
	'250': Day250,
	'255': Day255,
}

from d import mpDayAStrIn

for dayIn, dayFn in aDayInDayFn:

	mpDayFn[dayFn](mpDayAStrIn[dayIn])
	
	print '---'
