from pike import *
import os
from subprocess import call, Popen, PIPE
import tempfile
import shutil
import socket
import time
import glob

relpath = '../..'
exec(open(relpath + '/etc/make_site.py').read())

show_file = False

def getpf(pf):
    if 'tenancies' in globals():
        for tenant in tenancies:
            if tenancies[tenant].get('tenanttype', '') == 'Super':
                return '{0}_{1}.pf'.format(pf, tenant)
    return '{0}.pf'.format(pf)

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
    comp = Popen(mpro + ['-pf', '../../db/progress/store/all.pf', '-s', '120',
                         '-b', '-p', procedure_file.name], stdout=PIPE)
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


logging_level = '3'
extraargs = ['-logginglevel', logging_level, '-logthreshold', '500000']

@target
def run_agent(*a):
    if len(parameters) != 1:
        raise PikeException('Expected fcgi agent name as parameter')

    agent_name = parameters[0]
    
    os.environ['PROPATH'] += ',rpcmethods.pl'
    args = ['-pf', getpf('../../db/progress/store/all'), 
            '-clientlog', '../../var/log/%s_agent.%d.log' % \
            	          (agent_name, os.getpid())]
    args = mpro + args + extraargs + ['-b', '-p', 'fcgi_agent/nq_xmlrpc.p', '-param', ",YES"]
    os.execlp(args[0], *args)
