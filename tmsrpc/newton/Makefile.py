from pike import *
import os
from subprocess import Popen, PIPE
from ast import literal_eval

relpath = '../..'
exec(open(relpath + '/etc/make_site.py').read())
exec(open('../getpf.py').read())

def userandpass():
    if 'tenancies' in globals():
        for t, tdict in tenancies.items():
            if tdict['tenanttype'] == 'Super' or len(tenancies) == 1:
                return ['-U', '{0}@{1}'.format(tdict['username'], tdict['domain']), '-P', tdict['password'] ]
        raise ValueError('Cannot identify a super tenant')
    else:
        return []

def active_cdr_db_pf():
    if '-S' in open('../../db/progress/store/common.pf').read():
        connection_type = "tcp"
    else:
        connection_type = "local"

    args = ['-b', '-p', 'Syst/list_active_cdr_databases.p', '-param', connection_type]
    args.extend(['-pf', getpf('../../db/progress/store/common'), '-h', '1'])

    cdr_fetch = Popen(mpro + args, stdout=PIPE)
    dict = literal_eval(cdr_fetch.communicate()[0])

    if 'tenancies' in globals():
        uandp = userandpass()
        for db in dict:
            dict[db].extend(uandp)

    return dict

@target
def clean(*a):
    for file in glob('procore*'):
        os.unlink(file)


logging_level = '3'
extraargs = ['-logginglevel', logging_level, '-logthreshold', '1500000']

@target
def run_agent(*a):
    if len(parameters) != 1:
        raise PikeException('Expected fcgi agent name as parameter')

    agent_name = parameters[0]

    args = ['-pf', getpf('../../db/progress/store/all')]
    cdr_dict = {}
    dbcount = len(databases)

    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])
            dbcount += 1

    args.extend(['-h', str(dbcount + cdr_database_count * 2)])

    os.environ['PROPATH'] += ',rpcmethods.pl'
    args.extend(['-clientlog', '../../var/log/%s_agent.%d.log' % \
            	          (agent_name, os.getpid())])
    args = mpro + args + extraargs + ['-b', '-p', 'fcgi_agent/nq_xmlrpc.p']
    os.execlp(args[0], *args)
