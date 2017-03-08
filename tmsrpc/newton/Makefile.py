from pike import *
import os
from subprocess import call, Popen, PIPE
from ast import literal_eval
import tempfile
import shutil
import socket
import time
import glob

relpath = '../..'
exec(open(relpath + '/etc/make_site.py').read())

def write_version():
    fd = open('src/version.i', mode='wt')
    fd.write(appversion)
    fd.close()
if os.path.exists('src'):
    write_version()

show_file = False

def getpf(pf):
    if 'tenancies' in globals():
        for tenant in tenancies:
            if tenancies[tenant].get('tenanttype', '') == 'Super':
                return '{0}_{1}.pf'.format(pf, tenant)
    return '{}.pf'.format(pf)

def userandpass():
    if 'tenancies' in globals():
        for t, tdict in tenancies.items():
            if tdict['tenanttype'] == 'Super':
                return ['-U', '{0}@{1}'.format(tdict['username'], tdict['domain']), '-P', tdict['password'] ]
        raise ValueError('Cannot identify a super tenant')
    else:
        return []

def active_cdr_db_pf():
    if '-S' in open('../../db/progress/store/common.pf').read():
        connection_type = "tcp"
    else:
        connection_type = "local"

    args = ['-b', '-p', '../../tms/Syst/list_active_cdr_databases.p', '-param', connection_type]
    args.extend(['-pf', getpf('../../db/progress/store/common')])

    cdr_fetch = Popen(mpro + args, stdout=PIPE)
    dict = literal_eval(Popen('/bin/cat', stdin=cdr_fetch.stdout, stdout=PIPE).communicate()[0])

    if 'tenancies' in globals():
        uandp = userandpass()
        for db in dict:
            dict[db].extend(uandp)

    return dict

@target
def build(*a):
    if len(parameters) != 1:
        raise PikeException('Expected build_dir as parameter')
    build_dir = parameters[0]
    mkdir_p(build_dir)
    shutil.copy2('Makefile.py', build_dir)
    require('rpcmethods.pl', [])
    shutil.move('rpcmethods.pl', build_dir)

@target('compile')
def rpcmethods_pl(match, *a):
    """rpcmethods.pl"""
    if os.path.exists(match):
        os.unlink(match)
    call([dlc + '/bin/prolib', match, '-create'])
    for dir, _dirs, files in os.walk('rpcmethods'):
        for file in files:
            if file.endswith('.r') \
            or file.endswith('.help') \
            or file.endswith('.sig'):
                call([dlc + '/bin/prolib', match, '-add',
                      os.path.join(dir, file)])

@target
def compile(*a):
    mkdir_p('rpcmethods/doc')
    format = 'COMPILE %s SAVE INTO rpcmethods.'
    source_files = []
    for source_dir in [work_dir + '/tools/fcgi_agent/systemrpc',
                       os.getcwd() + '/src']:
        for file in os.listdir(source_dir):
            if file.endswith('.p'):
                source_files.append(os.path.join(source_dir, file))
    procedure_file = make_compiler(format, source_files,
                                       show='name' if show_file else '.')

    args = ['-pf', getpf('../../db/progress/store/all')]
    cdr_dict = {}

    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])

    comp = Popen(mpro + args + ['-s', '120', '-b', '-p', procedure_file.name], stdout=PIPE)

    call('/bin/cat', stdin=comp.stdout)
    if comp.wait() != 0:
        raise PikeException('Compilation failed')
    print('')
    helper_dir = work_dir + '/tools/fcgi_agent/xmlrpc/'
    for file in source_files:
        base = os.path.basename(file)
        if not base == 'system__multicall.p':
            fd = open('rpcmethods/doc/' + base.replace('.p', '.sig'), 'wb')
            call([sys.executable, helper_dir + 'signature.py', file], stdout=fd)
            fd.close()
        fd = open('rpcmethods/doc/' + base.replace('.p', '.help'), 'wb')
        call([sys.executable, helper_dir + 'help.py', file], stdout=fd)
        fd.close()


def make_compiler(cline, files, show='.'):
    compiler = tempfile.NamedTemporaryFile(suffix='.p', mode='wt+')
    compiler.write('ROUTINE-LEVEL ON ERROR UNDO, THROW.\n')
    for ff in files:
        if show == '.':
            compiler.write('PUT UNFORMATTED \'.\'.\n')
        elif show == 'name':
            compiler.write('PUT UNFORMATTED \'%s~n\'.\n' % ff)
        compiler.write(cline.replace('%s', ff) + '\n')
    compiler.flush()
    return compiler

def mkdir_p(directory):
    direntries = directory.split(os.path.sep)
    for dir in [os.path.sep.join(direntries[:ii+1]) \
                for ii in range(len(direntries))]:
        if dir and not os.path.exists(dir):
            os.mkdir(dir)

@target
def clean(*a):
    for file in glob('procore*'):
        os.unlink(file)


logging_level = '1'
extraargs = ['-logginglevel', logging_level, '-logthreshold', '500000']

@target
def run_agent(*a):
    if len(parameters) != 1:
        raise PikeException('Expected fcgi agent name as parameter')

    agent_name = parameters[0]

    args = ['-pf', getpf('../../db/progress/store/all')]
    cdr_dict = {}

    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])

    
    os.environ['PROPATH'] += ',rpcmethods.pl'
    args.extend(['-T', '../../var/tmp', '-clientlog', '../../var/log/%s_agent.%d.log' % \
            	          (agent_name, os.getpid())])
    args = mpro + args + extraargs + ['-b', '-p', 'fcgi_agent/nq_xmlrpc.p']
    os.execlp(args[0], *args)
