import re
import os

relpath = '../../..'
exec(open(relpath + '/etc/make_site.py').read())

from pike import *

tmscodes_file = relpath + '/db/progress/fixtures/tmscodes.yaml'
if not os.path.exists(tmscodes_file):
    raise PikeException('TMS codes not found in fixture directory')


@target
def any(match, deps, tbl, fld):
    '''(.*)\.(.*)'''
    print('TMS Codes for table %s field %s' % (tbl, fld))

    blockstart = re.compile('^.*: *TmsCodes *', re.I)
    matchers = (
            re.compile('^\s+TableName\s*:\s*["\']?(' + tbl + ')["\']?$', re.I),
            re.compile('^\s+FieldName\s*:\s*["\']?(' + fld + ')["\']?$', re.I),
            re.compile('^\s+CodeName\s*:\s*(.*)$', re.I),
            re.compile('^\s+CodeValue\s*:\s*(.*)$', re.I)
        )
    matches = [None, None, None, None]
    result = []

    for line in open(tmscodes_file):
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
