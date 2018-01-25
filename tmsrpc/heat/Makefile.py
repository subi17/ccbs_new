from pike import *
import os

relpath = '../..'
exec(open(relpath + '/etc/make_site.py').read())
exec(open('../getpf.py').read())

@target
def clean(*a):
    for file in glob('procore*'):
        os.unlink(file)


logging_level = '3'
extraargs = ['-logginglevel', logging_level, '-logthreshold', '500000']

@target
def run_agent(*a):
    if len(parameters) != 1:
        raise PikeException('Expected fcgi agent name as parameter')

    param = '{0}'.format('True' if environment == 'development' or environment == 'slavedevelopment' else 'False')

    os.environ['PROPATH'] += ',rpcmethods.pl'
    args = ['-pf', getpf('../../db/progress/store/all'),
            '-h', str(len(databases)),
            '-clientlog', '../../var/log/%s_agent.%d.log' % \
            	          (parameters[0], os.getpid())]

    args = mpro + args + extraargs + ['-b', '-p', 'fcgi_agent/nq_xmlrpc.p', '-param', param]
    os.execlp(args[0], *args)
