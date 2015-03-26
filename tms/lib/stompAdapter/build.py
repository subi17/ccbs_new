#!/usr/bin/env python

import os, shutil
from subprocess import Popen, PIPE
import tempfile

dlc = os.environ.get('DLC', '/opt/dlc/102a')
install_dir = os.environ.get('PREFIX', 'dist')
os.environ['PREFIX'] = install_dir
os.environ['PROPATH'] = 'stompAdapter'
os.environ['TERM'] = 'xterm'
os.environ['display_banner'] = 'no'

def ensure_installdir(dirname):
    dirname = os.path.join(install_dir, 'stompAdapter', dirname)
    for ii in range(dirname.count(os.path.sep) + 1):
        cdir = os.path.sep.join(dirname.split(os.path.sep)[:ii+1])
        if cdir == '': continue         # absolute path -> /
        if not os.path.exists(cdir):
            os.mkdir(cdir)

pfile = tempfile.NamedTemporaryFile(suffix='.p', mode='wt')
pfile.write('ROUTINE-LEVEL ON ERROR UNDO, THROW.\n')

for base, dirs, files in os.walk('stompAdapter'):
    ensure_installdir(base[13:])
    for file in files:
        if file.endswith('.cls'):
            pfile.write('COMPILE %s SAVE INTO %s.\n' % \
                               (os.path.join(base[13:], file),
                                os.path.join(install_dir, 'stompAdapter')))
        elif not file.endswith('.i') or file == 'Session.i':
            shutil.copyfile(os.path.join(base, file),
                            os.path.join(install_dir, base,file))
pfile.flush()
out = Popen([dlc+'/bin/mpro', '-b', '-p', pfile.name], stdout=PIPE)
errors = out.stdout.read()
if errors:
    raise Exception(errors)
