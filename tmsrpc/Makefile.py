from pike import *
import os
import shutil
from subprocess import call, Popen, PIPE
from socket import gethostname

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

state_base = os.path.abspath(os.path.join('..', 'var', 'run')) + '/'

def pikecheck():
    output = Popen(['which', 'pike'], stdout=PIPE, stderr=PIPE).communicate()
    pike = output[0].strip()
    if not pike:
        raise PikeException(output[1])
	return pike
	
pike = pikecheck()

@target('test>test')
def test(*a): pass

@target
def compile(*a):
    for rpc in parameters or rpcs.keys():
        require('%s>compile' % rpc)

#idname = 'lighttpd_tmsrpc'


@target(['%s_config' % rpc for rpc in rpcs])
def rundaemons(*a):
    call(['lighttpd', '-f', state_base + idname + '.conf'])
    pidfile = state_base + idname + '.pid'
    if not os.path.exists(pidfile):
        raise PikeException('Failed, no pidfile found')
    print('Daemon started')

@target
@applies_to(['%s_config' % rpc for rpc in rpcs])
def lighttpd_conf(match, *a):
    
	rpcname = match.split('_')[0]
	
	if environment == 'development':
	    configprefix = 'development'
    else:
	    configprefix = gethostname()

	if not os.path.exists(rpcname + '/' + configprefix + '.config.py'):
        return

    username  = None
    groupname = None
	
	exec(open(rpcname + '/' + configprefix + '.config.py').read())
		
    for prt in port:
        with open(state_base + 'lighttpd_' + rpcname + '_' + port + '.conf', 'wt') as fd:
  	        fd.write('''#
# Auto-generated lighttpd-conf
#

server.modules          = ( "mod_fastcgi", "mod_status", "mod_accesslog" )
var.approot             = "{0}/tmsrpc"
var.appstate            = "{0}/var"
server.document-root    = "/var/tmp"
server.port             = {1}
server.errorlog         = var.appstate + "/log/{2}_{1}_error.log"
accesslog.filename      = var.appstate + "/log/{2}_{1}_access.log"
server.pid-file         = var.appstate + "/run/{2}_{1}.pid"
accesslog.format        = "%h %V %u %t \\"%r\\" %>s %b \\"%{{Referer}}i\\"  \\"%{{User- Agent}}i\\" %T"
'''.format(work_dir, prt, rpcname))

            if username != None:
		        fd.write('server.username         = "%s"\n' % username)

            if groupname != None:
		    fd.write('server.username         = "%s"\n' % groupname)

        fd.write('fastcgi.server = (\n')
        for name, url in rpcs.items():
            fd.write('''
  "{1}" => (
    ("bin-path"     => "{2} -C " + var.approot + "/{0} run_agent",
     "socket"       => var.appstate + "/run/{0}.socket",
     "check-local"  => "disable",
     "min-procs"    => 4,
     "max-procs"    => 4),
  )
)\n

$HTTP["remoteip"] == "127.0.0.1" {{status.status-url = "/server-status"}}

'''.format(name, url, pike))

@target
def daemonsstatus(*a):
    pidfile = state_base + idname + '.pid'
    if not os.path.exists(pidfile):
        print('Daemon is not running')
    elif os.path.exists(pidfile):
        fd = open(pidfile, 'rt')
        pid = fd.read().strip()
        fd.close()
        print('Daemon running with pid %s' % pid)

@target
def stopdaemons(*a):
    pidfile = state_base + idname + '.pid'
    if not os.path.exists(pidfile):
        raise PikeException('Pidfile %s not found' % pidfile)
    fd = open(pidfile, 'rt')
    pid = fd.read().strip()
    fd.close()
    call(['kill', '-2', pid])
    print('Kill signal sent to pid %s' % pid)

@target
def build(*a):
    if len(parameters) != 1:
        raise PikeException('Expected build_dir as parameter')
    build_dir = parameters[0]
    if not os.path.exists(build_dir):
        os.mkdir(build_dir)
    shutil.copy('Makefile.py', build_dir)
    for rpc in rpcs.keys():
        require('%s>build' % rpc, [os.path.join(build_dir, rpc)])

