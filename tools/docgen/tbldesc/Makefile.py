from subprocess import Popen, PIPE

relpath = '../../..'
exec(open(relpath + '/etc/make_site.py').read())
from pike import *


@target
def any(match, deps):
    '''.*'''
    for db in databases:
        x = Popen(mpro + ['-pf', relpath + '/db/progress/store/%s.pf' % db,
                          '-b', '-p', 'protabledesc.p',
                          '-param', match], stdout=PIPE)
        output, _err = x.communicate()
        if x.poll() != 0:
            print('Error : ' + output)
        if output:
            print(output.replace('Table description for ',
                                 'Table description for ' + db + '.'))
            break
    else:
        raise PikeException('Table %s not found in %d databases' % \
                            (match, len(databases)))
