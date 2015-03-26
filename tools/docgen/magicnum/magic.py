import sys
import re

assert len(sys.argv) == 4, 'Insufficient commandline parameters'
yamlfile, tn, fn = sys.argv[1:]

blockstart = re.compile('^.*: *TmsCodes *', re.I)
matchers = (
        re.compile('^\s+TableName\s*:\s*["\']?(' + tn + ')["\']?$', re.I),
        re.compile('^\s+FieldName\s*:\s*["\']?(' + fn + ')["\']?$', re.I),
        re.compile('^\s+CodeName\s*:\s*(.*)$', re.I),
        re.compile('^\s+CodeValue\s*:\s*(.*)$', re.I)
    )
matches = [None, None, None, None]
result = []

for line in open(yamlfile):
    if blockstart.match(line):
        matches = [None, None, None, None]
        continue
    for matcher, match, index in ((x, x.match(line).group(1), i) \
                                  for i, x in enumerate(matchers) \
                                  if x.match(line)):
        matches[index] = match.strip('"\'')
        if not None in matches:
            result.append((matches[3], matches[2]))
            matches = [None, None, None, None]

for key, val in result:
    print('%20s = %s' % (key, val))
