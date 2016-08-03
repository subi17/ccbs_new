from pike import *
import os
import shutil
import json
from subprocess import call, Popen, PIPE
from socket import gethostname
from string import Template

def pikecheck():
    output = Popen(['which', 'pike'], stdout=PIPE, stderr=PIPE).communicate()
    pike = output[0].strip()
    if not pike:
        raise PikeException(output[1])
    return pike

pike = pikecheck()

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

if environment == 'development':
    configfile = 'development_config.json'
else:
    configfile = gethostname() + '_config.json'

if not os.path.exists(configfile):
    raise PikeException('Failed, no lighttpd configuration file ' + configfile + ' found')	

state_base = os.path.abspath(os.path.join('..', 'var', 'run')) + '/'

lighttpd_template = Template('''server.document-root = "/var/tmp"
server.errorlog = "${work_dir}/var/log/lighttpd_${rpc}_${port}_error.log"
accesslog.filename = "${work_dir}/var/log/lighttpd_${rpc}_${port}_access.log"
server.pid-file = "${work_dir}/var/run/lighttpd_${rpc}_${port}.pid"
server.port = $port
''')

fastcgi_template = Template('''    ("bin-path" => "${pike} -C ${work_dir}/tmsrpc/${fcgi} run_agent ${fcgi}",
     "socket" => "${work_dir}/var/run/${fcgi}-${port}.socket",
''')

def do_lighttpd_conf(a):

    with open(configfile, 'r') as jsonfile:
        jsondata = json.load(jsonfile)
        for rpc in jsondata:
            if not a or rpc in a:
                for port in jsondata[rpc]:

                    lighttpdparams = (param for param in jsondata[rpc][port] if not param.startswith('/'))
                    fcgis = (param for param in jsondata[rpc][port] if param.startswith('/'))

                    with open(state_base + 'lighttpd_' + rpc + '_' + port + '.conf', 'wt') as fd:

                        fd.write('# Auto-generated lighttpd-conf\n\n')

                        for lighttpdparam in lighttpdparams:
                            fd.write('{0} = {1}\n'.format(lighttpdparam, jsondata[rpc][port][lighttpdparam]))

                        fd.write(lighttpd_template.substitute(work_dir=work_dir, rpc=rpc, port=port))

                        fd.write("\nfastcgi.server = (\n")

                        fcgi_end = ''
                        for fcgi in fcgis:
                            fd.write(fcgi_end + '  "{0}" => (\n'.format(fcgi) )
                            fd.write(fastcgi_template.substitute(pike=pike, work_dir=work_dir, fcgi=jsondata[rpc][port][fcgi][0], port=port))

                            fcgiparam_end = ''
                            for fcgiparam, fcgivalue in jsondata[rpc][port][fcgi][1].items():
                               fd.write(fcgiparam_end + '     "{0}" => {1}'.format(fcgiparam, fcgivalue))
                               fcgiparam_end = ',\n'

                            if not fcgiparam_end == '':
                               fd.write('),\n')

                            fcgi_end = '  ),\n'

                        if not fcgi_end == '':
                            fd.write('  )\n')

                        fd.write(')\n')

                        fd.close()
        
        jsonfile.close()

def lighttpd_gen(a):
    with open(configfile, 'r') as jsonfile:
        jsondata = json.load(jsonfile)
        for rpc in jsondata:
            if not a or rpc in a:
                for port in jsondata[rpc]:
                    yield 'lighttpd_{0}_{1}'.format(rpc, port)
        jsonfile.close()

@target('test>test')
def test(*a): pass

@target
def compile(*a):
    for rpc in parameters or rpcs.keys():
        require('%s>compile' % rpc)

@target
def start(*a):
    '''start|run|rundaemons'''
    do_lighttpd_conf(parameters)

    for lighttpd in lighttpd_gen(parameters):
        pidfile = state_base + lighttpd + '.pid'
        if os.path.exists(pidfile):
            print('No need to start already running daemon ' + lighttpd)
        else:
            call(['lighttpd', '-f', state_base + lighttpd + '.conf'])
            if not os.path.exists(pidfile):
                raise PikeException('Failed, no pidfile ' + pidfile + ' found')
            print('STARTED daemon ' + lighttpd)

@target
def status(*a):
    '''status|daemonsstatus|daemonstatus'''
    for lighttpd in lighttpd_gen(parameters):
        pidfile = state_base + lighttpd + '.pid'
        if not os.path.exists(pidfile):
            print('Daemon ' + lighttpd + ' is not running')
        else:
            fd = open(pidfile, 'rt')
            pid = fd.read().strip()
            fd.close()
            print('Daemon ' + lighttpd + ' is running with pid %s' % pid)
            
@target
def stop(*a):
    '''stop|stopdaemons'''
    for lighttpd in lighttpd_gen(parameters):
        pidfile = state_base + lighttpd + '.pid'
        if os.path.exists(pidfile):
            fd = open(pidfile, 'rt')
            pid = fd.read().strip()
            fd.close()
            call(['kill', '-2', pid])
            print('Kill signal sent to daemon ' + lighttpd + ' pid %s' % pid)

@target
def build(*a):
    if len(parameters) != 1:
        raise PikeException('Expected build_dir as parameter')
    build_dir = parameters[0]

    if build_dir[0] != "/":
        build_dir = os.getcwd() + '/' + build_dir

    if not os.path.exists(build_dir):
        os.mkdir(build_dir)
    shutil.copy('Makefile.py', build_dir)

    for file in glob('*_config.json'):
        shutil.copy(file, build_dir)

    for rpc in rpcs.keys():
        require('%s>build' % rpc, [os.path.join(build_dir, rpc)])
