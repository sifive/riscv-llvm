suffixMap = {
  '.adb'   : 'ada',
  '.ads'   : 'ada',
  '.c'     : 'c',
  '.i'     : 'c',
  '.C'     : 'c++',
  '.cc'    : 'c++',
  '.cpp'   : 'c++',
  '.ii'    : 'c++',
  '.f'     : 'fortran',
  '.f03'   : 'fortran',
  '.f08'   : 'fortran',
  '.f90'   : 'fortran',
  '.f95'   : 'fortran',
  '.F'     : 'fortran',
  '.F03'   : 'fortran',
  '.F08'   : 'fortran',
  '.F90'   : 'fortran',
  '.F95'   : 'fortran',
  '.go'    : 'go',
  '.jar'   : 'java',
  '.class' : 'java',
  '.m'     : 'objc',
  '.mm'    : 'objc++',
}

def getLanguageForSuffix(suffix):
    return suffixMap[suffix]

def getSuffixesForLanguage(language):
    suffixes = []
    for suffix in suffixMap:
       if suffixMap[suffix] == language:
           suffixes.append(suffix)
    return suffixes
