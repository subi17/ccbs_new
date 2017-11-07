from pike import *
from subprocess import Popen, PIPE, call
import re

relpath = '../../../..'
exec(open(relpath + '/etc/make_site.py').read())

@target('dump_dicts.p')
def dicts(*a):
    if os.path.exists('dict'):
        os.unlink('dict')
    if os.path.exists('tabledict'):
        os.unlink('tabledict')
    for db in databases:
        call(mpro + ['-pf', '../{0}.pf'.format(db),
                     '-b', '-p', './dump_dicts.p'])

@file_target
def dump_dicts(*a):
    """dump_dicts.p"""
    open('dump_dicts.p', 'wt').write('''
DEF STREAM sDict.
DEF STREAM sTableDict.

OUTPUT STREAM sDict TO dict APPEND.
OUTPUT STREAM sTableDict TO tabledict APPEND.

FOR EACH _file NO-LOCK
WHERE NOT _file._file-name BEGINS "_"
  AND NOT _file._file-name BEGINS "SYS":
   PUT STREAM sTableDict UNFORMATTED LDBNAME(1) + "." + _file._file-name + "~n".
   FOR EACH _field OF _file NO-LOCK:
      PUT STREAM sDict UNFORMATTED _file._file-name + "." +
                                   _field._field-name + ";" + 
                                   _field._data-type.
      IF _field._extent NE 0 THEN
         PUT STREAM sDict UNFORMATTED SUBST("(&1)", _field._extent).
      PUT STREAM sDict UNFORMATTED ';FORMAT "' + _field._format + '"~n'.
   END.
END.

OUTPUT STREAM sDict CLOSE.
OUTPUT STREAM sTableDict CLOSE.
''')
