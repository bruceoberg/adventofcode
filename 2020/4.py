#!python

import re

with open('4.txt') as f:
    lStrFile = f.read().split('\n\n')

lStrPasp = [strLine.replace('\n', ' ') for strLine in lStrFile]

def PaspFromStr(strPasp):
    return {strKey: strVal for strWord in strPasp.split() for strKey, strVal in [strWord.split(':')]}

lPasp = [PaspFromStr(strPasp) for strPasp in lStrPasp]


def FIsByrValid(strVal):
    return 1920 <= int(strVal) <= 2002


def FIsIyrValid(strVal):
    return 2010 <= int(strVal) <= 2020


def FIsEyrValid(strVal):
    return 2020 <= int(strVal) <= 2030


def FIsHgtValid(strVal):
    if strVal[-2:] not in ['in', 'cm']:
        return False
    if strVal[-2:] == 'in':
        return 59 <= int(strVal[:-2]) <= 76
    elif strVal[-2:] == 'cm':
        return 150 <= int(strVal[:-2]) <= 193


def FIsHclValid(strVal):
    m = re.search(r'\#[0-9a-f]{6}', strVal)
    return m is not None


def FIsEclValid(strVal):
    return strVal in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']


def FIsPidValid(strVal):
    m = re.search(r'^[0-9]{9}$', strVal)
    return m is not None


def FIsCidValid(strVal):
    return True


mpStrKeyFnValid = {
    'byr': FIsByrValid,
    'iyr': FIsIyrValid,
    'eyr': FIsEyrValid,
    'hgt': FIsHgtValid,
    'hcl': FIsHclValid,
    'ecl': FIsEclValid,
    'pid': FIsPidValid,
    'cid': FIsCidValid,
}


def FIsPaspValid(pasp):
    for strKey, fnIsValid in mpStrKeyFnValid.items():
        if strKey not in pasp.keys() and strKey != 'cid':
            return 0
    return 1


def FIsPaspLegit(pasp):
    for strKey, fnIsValid in mpStrKeyFnValid.items():
        if strKey not in pasp.keys() and strKey != 'cid':
            return 0
        if strKey != 'cid':
            if not fnIsValid(pasp[strKey]):
                return 0
    return 1

def FIsStrPaspLegit(strPasp):
    return FIsPaspLegit(PaspFromStr(strPasp))



lPaspValid = filter(FIsPaspValid, lPasp)
lPaspLegit = filter(FIsPaspLegit, lPasp)
lStrPaspLegit = filter(FIsStrPaspLegit, lStrPasp)

print(len(lPasp))
#print(sum(FIsPaspValid(pasp) for pasp in lPasp))
print(len(lPaspValid))
#print(sum(FIsPaspLegit(pasp) for pasp in lPasp))
print(len(lPaspLegit))
print(len(lStrPaspLegit))

with open('4po.txt', 'wt') as f:
    f.write('\n'.join(lStrPaspLegit))

