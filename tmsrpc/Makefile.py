from pike import *
import os
import shutil
from subprocess import call, Popen, PIPE

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

@target('test>test')
def test(*a): pass

@target
def compile(*a):
    for rpc in parameters or rpcs.keys():
        require('%s>compile' % rpc)

idname = 'lighttpd_tmsrpc'
state_base = os.path.abspath(os.path.join('..', 'var', 'run')) + '/'
@target('lighttpd_conf')
def rundaemons(*a):
    call(['lighttpd', '-f', state_base + idname + '.conf'])
    pidfile = state_base + idname + '.pid'
    if not os.path.exists(pidfile):
        raise PikeException('Failed, no pidfile found')
    print('Daemon started')
   
@file_target
def lighttpd_conf(*a):
    if environment != 'development':
        raise PikeException('rundaemons not allowed in %s, check runbooks' % environment)
    output = Popen(['which', 'pike'], stdout=PIPE, stderr=PIPE).communicate()
    pike = output[0].strip()
    if not pike:
        raise PikeException(output[1])
    with open(state_base + idname + '.conf', 'wt') as fd:
        rpc_conf.setdefault('port', 3001)
        rpc_conf.setdefault('min_agents', 4)
        rpc_conf.setdefault('max_agents', 4)
        fd.write('''#
# Auto-generated lighttpd-conf
#

server.modules          = ( "mod_fastcgi", "mod_accesslog", "mod_setenv", "mod_status" )
var.approot             = "{0}/tmsrpc"
var.appstate            = "{0}/var"
server.document-root    = "/var/tmp"
server.port             = {1}
server.errorlog         = var.appstate + "/log/{2}_error.log"
accesslog.filename      = var.appstate + "/log/{2}_access.log"
accesslog.format        = "%h %V %u %t \\"%r\\" %>s %b \\"%{{Referer}}i\\"  \\"%{{User- Agent}}i\\" %T"
server.pid-file         = var.appstate + "/run/{2}.pid"
'''.format(work_dir, rpc_conf['port'], idname))
        if rpc_conf.get('run_as', None):
            name = rpc_conf['run_as']
            fd.write('server.username         = "%s"\n' % name)
            fd.write('server.groupname         = "%s"\n' % name)
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

