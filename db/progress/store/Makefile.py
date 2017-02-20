from pike import *
from socket import gethostname, getservbyname
from ast import literal_eval
from subprocess import call, Popen, PIPE
from os import getcwd
import os, sys
import tempfile
import shutil
import time

# NOTE: This file is also used by tms/test/db
relpath = '../../..'
exec(open(relpath + '/etc/make_site.py').read())

if not 'tenancies' in globals():
    global tenancies
    tenancies = {}

########################## Configuration #############################

# Configure database location (on different partitions/remote hosts)
# for production servers. All databases not found in this dictionary
# will be assumed/created in the current directory

db_locations = {
    'alpheratz': {'common': '/db1/common/common',
                  'ordercanal': '/db1/ordercanal/ordercanal',
                  'mobile': '/db1/mobile/mobile',
                  'counter': '/db1/counter/counter',
                  'star': '/db1/star/star',
                  'prepedr': '/db1/prepedr/prepedr',
                  'fraudcdr': '/db1/fraudcdr/fraudcdr',
                  'reratelog': '/db1/reratelog/reratelog'},
    'yanai': {'common': '/db1/common/common',
              'ordercanal': '/db1/ordercanal/ordercanal',
              'mobile': '/db1/mobile/mobile',
              'counter': '/db1/counter/counter',
              'star': '/db1/star/star',
              'prepedr': '/db1/prepedr/prepedr',
              'fraudcdr': '/db1/fraudcdr/fraudcdr',
              'reratelog': '/db1/reratelog/reratelog'},
    'arneb': {'common': '/db1/common/common',
              'ordercanal': '/db1/ordercanal/ordercanal',
              'mobile': '/db1/mobile/mobile',
              'counter': '/db1/counter/counter',
              'star': '/db1/star/star',
              'prepedr': '/db1/prepedr/prepedr',
              'fraudcdr': '/db1/fraudcdr10/fraudcdr10',
              'reratelog': '/db1/reratelog/reratelog'},
    'pallas': {'common': '/db1/common/common',
               'ordercanal': '/db1/ordercanal/ordercanal',
               'mobile': '/db1/mobile/mobile',
               'counter': '/db1/counter/counter',
               'star': '/db1/star/star',
               'prepedr': '/db1/prepedr/prepedr',
               'fraudcdr': '/db1/fraudcdr10/fraudcdr10',
               'reratelog': '/db1/reratelog/reratelog'},
    'leto': {'common': 'pallas.int.asp.qvantel.net:common',
             'ordercanal': 'pallas.int.asp.qvantel.net:ordercanal',
             'mobile': 'pallas.int.asp.qvantel.net:mobile',
             'counter': 'pallas.int.asp.qvantel.net:counter',
             'star': 'pallas.int.asp.qvantel.net:star',
             'prepedr': 'pallas.int.asp.qvantel.net:prepedr'},
    'maja': {'common': 'pallas.int.asp.qvantel.net:common',
             'ordercanal': 'pallas.int.asp.qvantel.net:ordercanal',
             'mobile': 'pallas.int.asp.qvantel.net:mobile',
             'counter': 'pallas.int.asp.qvantel.net:counter',
             'star': 'pallas.int.asp.qvantel.net:star',
             'prepedr': 'pallas.int.asp.qvantel.net:prepedr'}
}

db_processes = {'common': ['biw', 'wdog', ('apw', 4)],
                'ordercanal': ['biw', 'wdog', ('apw', 4)],
                'mobile': ['biw', 'wdog', ('apw', 4)],
                'counter': ['biw', 'wdog', ('apw', 4)],
                'star': ['biw', 'wdog', ('apw', 4)],
                'prepedr': ['biw', 'wdog', ('apw', 4)],
                'fraudcdr': ['biw', 'wdog', ('apw', 4)],
                'reratelog': ['biw', 'wdog', ('apw', 4)]
}

process_name = {'biw': 'Before-Image writer',
                'apw': 'Asynchronous page writer',
                'wdog': 'user watchdog',
                'sql': 'SQL server'}

blocksize = 8

# Additional commandline parameters for database startup
# These are added in production mode only
db_startup_options = {
#    'db_one': ['-spin 5000']
}

# Note that -ServerType SQL is automatically added later
sql_startup_options = {
#    'db_one': ['-spin 5000']
}

# All .pf files that are used by the clients. You need to define
# producing target functions below for each added file. Check all_pf!
main_pf_files = ['all.pf']

########################## Implementation #############################

try:
    getservbyname('%s%s' % (databases[0], service_suffix), 'tcp')
    db_by_unix_socket = False
except:
    db_by_unix_socket = True

initialize_dependencies = list(main_pf_files)
if environment == 'development':
    initialize_dependencies += ['create', 'start', 'migrate', 'tenanciescreate', 'fixtures']
elif environment == 'production':
    initialize_dependencies += ['relink_migcache']

def db_full_path(db_name, suffix='.db', host=None):

    if environment == 'development':
        return '{0}/{1}{2}'.format(getcwd(), db_name, suffix)

    locs = {}

    host = host or gethostname()
    assert host in db_locations, 'Db locations not configured for this host'

    locs = db_locations[host]

    return '{0}{1}'.format(locs.get(db_name, '{0}/{1}'.format(getcwd(), db_name)), suffix)

@target(initialize_dependencies)
def initialize(*a): pass

@target
def relink_migcache(*a):
    os.rmdir(migcache_dir)
    migcache_real = os.path.abspath(work_dir + '/../var/migrations')
    if not os.path.exists(migcache_real):
        os.mkdir(migcache_real)
    os.symlink(migcache_real, migcache_dir)


@target([db_full_path(x) for x in parameters or databases])
def create(match, deps): pass


@file_target([r'\1/\2.st'])
def database_file(match, deps, db_dir, db_name):
    '''([-_/a-zA-Z0-9.]+)/([_a-zA-Z0-9]+)\.db'''

    if environment != 'development':
        raise PikeException('Currently creating database is only allowed in development mode.')

    print('Creating database %s...' % db_name)
    callgrep([dlc + '/bin/prostrct', 'create', match[:-3],
                                 '-blocksize', str(blocksize * 1024)],
            ['Procopy session begin', 'Formatting extents:',
             'size +area name +path name', '^$',
             '/(bi|db|ix)/%s(_\d+)?\.[dba]\d+ 00:00:00' % db_name])
    for emptydb in ['%s/prolang/%s/empty%d.db' % (dlc, x, blocksize) \
                    for x in ('utf', 'eng')]:
        if os.path.exists(emptydb):
            callgrep([dlc + '/bin/procopy', '-s', emptydb, match],
                    ['\(6715\)$', '\(6718\)$', '\(451\)$',
                     '0 Percent complete.', '^$',
                     '\(6720\)$', '\(6722\)$', '\(1365\)$', '\(334\)$'])
            if tenancies:
                callgrep([dlc + '/bin/proutil', match, '-C', 'enablemultitenancy'], ['Multi'])
            break
    else:
        raise StandardError('No empty db template found')

@file_target
def structure_file(match, deps, db_dir, db_name):
    '''([-_/a-zA-Z0-9.]+)/([_a-zA-Z0-9]+)\.st'''

    if environment != 'development':
        raise PikeException('Currently creating a .st file is only allowed in development mode.')

    fd = open(match, 'w')
    if environment == 'development':
        _ = lambda x: fd.write(x % getcwd() + '\n#\n')
        _('b %s/bi')
        _('d "Schema Area" %s/db')
        for size in (64, 128, 256):
            _('d "Sta_Data_%d",%d;8 %%s/db' % (size, size))
        _('d "Sta_Index_64",64;8 %s/ix')
        for dir in ['bi', 'db', 'ix']:
            if not os.path.exists(dir): os.mkdir(dir)
    else:
        basedir = os.path.dirname(db_full_path(db_name))
        referred_dirs = []
        for line in open(work_dir + '/db/progress/migrations/%s.st' % db_name):
            fd.write(line.replace('BASEDIR', basedir))
            # TODO create directories mentioned in .st file
    fd.close()
        

@target
def remote_database_file(match, deps, host, db_dir, db_name):
    '''([a-zA-Z]+):([-_/a-zA-Z0-9]+)/([_a-zA-Z0-9]+).db'''
    print('Remote creation of database %s in %s on %s not implemented' % \
            (db_name, db_dir, host))

def write_pf_file(filename, tenant='', logical_names={}):
    if tenant:
        tenant = '_{}'.format(tenant)
    with open(filename, 'wt') as fd:
        fd.write('-h %d\n' % len(databases))
        for db in databases:
            name_map = ' -ld %s' % logical_names[db] if db in logical_names else ''
            fd.write('-pf {0}/{1}{2}.pf{3}\n'.format(getcwd(), db, tenant, name_map))

@target(['{}.pf'.format(x) for x in databases] + [ 'tenant_{}'.format(x) for x in tenancies ])
def all_pf(match, deps):
    '''all\.pf'''
    write_pf_file(match)
    for tenant in tenancies:
        write_pf_file('all_{}.pf'.format(tenant), tenant)

@target(r'\1.pf')
def startup_parameter_file(match, deps, db_name):
    '''([_a-zA-Z0-9]+)_startup\.pf'''
    path = db_full_path(db_name, '').split(':')
    if len(path) > 1:
        return False
    path = path[0]
    fd = open(match, 'wt')
    fd.write('-pf %s/etc/pf/formats.pf\n' % work_dir)
    if environment == 'production':
        fd.write('-db %s\n' % path)
        if not db_by_unix_socket:
            fd.write('-H %s\n' % gethostname())
            fd.write('-S %s%s\n' % (db_name, service_suffix))
        for option in db_startup_options.get(db_name, []):
            fd.write(option + '\n')
    else:
        fd.write('-db %s\n' % path)
    fd.close()
    if environment == 'production' and db_name in sql_startup_options:
        fd = open(db_name + '_sql_startup.pf', 'wt')
        fd.write('-pf %s/etc/pf/formats.pf\n' % work_dir)
        fd.write('-db %s\n' % path)
        fd.write('-H %s\n' % gethostname())
        fd.write('-S %s_sql%s\n' % (db_name, service_suffix))
        fd.write('-ServerType SQL\n')
        for option in sql_startup_options.get(db_name, []):
            fd.write(option + '\n')
        fd.close()

@target
def connect_parameter_file(match, deps, db_name):
    '''([_a-zA-Z0-9]+)\.pf'''
    path = db_full_path(db_name, '').split(':')

    fd = open(db_name + '.pf', 'wt')
    if len(path) > 1:
        fd.write('-db %s\n' % path[1].split('/')[0])
        fd.write('-H %s\n' % path[0])
        fd.write('-S %s%s\n' % (db_name, service_suffix))
    else:
        fd.write('-db %s\n' % path[0])
    fd.close()
    for tenant in tenancies:
        with open('{0}_{1}.pf'.format(db_name, tenant), 'wt') as fd:
            fd.write('-pf {}.pf\n'.format(path[0]))
            fd.write('-pf {0}/tenant_{1}.pf\n'.format(getcwd(), tenant))

@target
@applies_to(['tenant_none'] + [ 'tenant_{}'.format(x) for x in tenancies ])
def tenantcredentials_file(match, deps):
    tenant = match.split('_')[1]
    if tenant in tenancies:
        with open('tenant_{}.pf'.format(tenant), 'wt') as fd:
            fd.write('-U {0}@{1}\n'.format(tenancies[tenant]['username'], tenancies[tenant]['domain']))
            fd.write('-P {}\n'.format(tenancies[tenant]['password']))

db_running_msg = True

@target(['%s_startup.pf' % x for x in parameters or databases])
def start(match, deps):

    if environment != 'development':
        raise PikeException('Currently starting a database via pike is only allowed in development mode.')

    for db in parameters or databases:
        path = db_full_path(db, '.lk')
        if path.find(':') > -1:
            print('Skipping remote database %s' % db)
        elif os.path.exists(path):
            if db_running_msg:
                print('Database %s is already running' % db)
        else:
            print('Starting database %s...' % db)
            ec = callgrep([dlc + '/bin/proserve', '-pf', db + '_startup.pf'],
                     ['^OpenEdge Release'] +
                        ['\(%d\)$' % x for x in [333, 5326, 7161, 13547]])
            if ec == 0 and environment == 'production':
                start_db_processes(db)

def start_db_processes(db):
    for ii in db_processes.get(db, []):
        if isinstance(ii, tuple):
            process, amount = ii
        else:
            process, amount = ii, 1
        name = process_name[process]
        article = 'an' if name[0].lower() in 'aeiou' else 'a'
        if process == 'sql':
            print('  adding {0} {1}...'.format(article, name))
            callgrep([dlc + '/bin/proserve', '-pf', db + '_sql_startup.pf'],
                     [])
        else:
            for _num in range(amount):
                print('  adding {0} {1}...'.format(article, name))
                callgrep([dlc + '/bin/pro' + process, db_full_path(db)], [])


@target(['%s.pf' % x for x in parameters or databases])
def stop(match, deps):

    if environment != 'development':
        raise PikeException('Currently stopping a database via pike is only allowed in development mode.')

    for db in parameters or databases:
        path = db_full_path(db, '.lk')
        if path.find(':') > -1:
            print('Skipping remote database %s' % db)
        elif not os.path.exists(path):
            print('Database %s is not running' % db)
        else:
            print('Stopping database %s...' % db)
            callgrep([dlc + '/bin/proshut', '-by', '-pf', db + '.pf'],
                    ['Shutdown is executing'])


@target(['stop'])
def clean(match, deps):

    if environment != 'development':
        raise PikeException('Deleting a database via pike is only allowed in development mode.')

    for db in parameters or databases:
        path = os.path.dirname(db_full_path(db))
        if path.find(':') > -1:
            print('Cannot clean remote database %s' % db)
            continue
        sys.stderr.write("Press enter if you want to remove database %s. " % db)
        sys.stderr.flush()
        try:
            sys.stdin.readline()
        except KeyboardInterrupt:
            print('')
            raise PikeException('Database NOT removed')
        s = (path, db)
        for file in glob('%s/%s.*' % s) + ['%s/%s_startup.pf' % s] + \
                    glob('%s/bi/%s.*' % s) + glob('%s/db/%s.*' % s) + \
                    glob('%s/db/%s_*' % s) + glob('%s/ix/%s_*' % s) + \
                    glob('migrations/*_%s_*' % db):
            if os.path.exists(file):
                os.unlink(file)
        print('Database %s deleted' % db)


# Migrations

migrations_dir = os.path.join(work_dir, 'db', 'progress', 'migrations')
migcache_dir   = 'migrations'
if not os.path.exists(migcache_dir): os.mkdir(migcache_dir)
avail_migrations = sorted((x.group(1), int(x.group(2))) \
                     for x in [re.match(r'^((\d+)_.*)\.py$', y) \
                               for y in os.listdir(migrations_dir)] if x)
def done_migrations():
    return sorted(x[:-3] for x in os.listdir(migcache_dir) if x.endswith('.py'))

from gearbox.migrations.mig2df import *

def a2t_from_database(database):
    script = 'gearbox/migrations/dump_areas'
    if os.path.exists('{0}/tools/{1}.r'.format(work_dir, script)):
        os.unlink('{0}/tools/{1}.r'.format(work_dir, script))
    x = Popen(mpro + ['-pf', '{0}.pf'.format(database),
                 '-b', '-p', script + '.p'], stdout=PIPE)
    if x.wait() != 0:
        print(x.stdout.read())
        raise PikeException('Unable to read schema data from '
                            'database {0}'.format(database))
    return split_a2t_data((x.decode('utf-8') for x in x.stdout.readlines()))


def _migrate(migration_file, direction, a2t_data):
    assert os.access(migcache_dir, os.W_OK), \
           'Migration-cache directory is not writable'

    df_file, database = mig2df(migration_file, direction,
                               a2t_data, a2t_from_database, environment)

    pfile = tempfile.NamedTemporaryFile(suffix='.p', mode='wt+')
    pfile.write('SESSION:SUPPRESS-WARNINGS = TRUE.\n')
    pfile.write('SESSION:NUMERIC-FORMAT = "American".\n')
    pfile.write('RUN prodict/load_df("{0},,").\n'.format(df_file.name))
    pfile.flush()

    callgrep(mpro + ['-pf', database + '.pf', '-b', '-p', pfile.name], [])

    error_file = database + '.e'
    if os.path.exists(error_file):
        error = open(error_file).read().replace('\n\n', '\n')
        os.unlink(error_file)
        raise PikeException('Migration %s failed:\n%s' % \
                            (os.path.basename(migration_file), error))

    if direction == 'up':
        shutil.copyfile(migration_file,
                        'migrations/' + os.path.basename(migration_file))
    else:
        os.unlink(migration_file)
    log_line = '%s %s %-4s' % (time.strftime('%FT%T'), os.environ['USER'], direction)
    open('migrations/' + os.path.basename(migration_file) + '.history', 'a').write(log_line)

@target
def migrate(match, deps):

    if environment == 'safeproduction':
        raise PikeException('Safe production mode. Migration features are disabled.')

    version = int(parameters[0]) if parameters else 99999
    done_migs = done_migrations()

    to_downgrade = [m for m in done_migs \
                    if not m in (x[0] for x in avail_migrations)]
    to_upgrade = []
    for name, number in avail_migrations:
        if number > version:
            if name in done_migs:
                to_downgrade.append(name)
        else:
            if not name in done_migs:
                to_upgrade.append(name)

    a2t_cache = {}
    for mig in reversed(sorted(to_downgrade)):
        _migrate(migcache_dir + '/' + mig + '.py', 'down', a2t_cache)
    for mig in sorted(to_upgrade):
        _migrate(migrations_dir + '/' + mig + '.py', 'up', a2t_cache)
    if environment == 'development':
        require('cache>dicts')

@target
def status(match, deps):

    if environment == 'safeproduction':
        raise PikeException('Safe production mode. Migration features are disabled.')

    done_migs = done_migrations()
    for name, number in avail_migrations:
        if name in done_migs:
            print('%-40s: Done' % name.replace('_', ' '))
        else:
            print('%-40s: Not yet' % name.replace('_', ' '))
    for name in [m for m in done_migs \
                   if not m in (x[0] for x in avail_migrations)]:
        print('%-40s: Orphaned' % name.replace('_', ' '))

@target
def history(match, deps):

    if environment == 'safeproduction':
        raise PikeException('Safe production mode. Migration features are disabled.')

    data = []
    for file in os.listdir(migcache_dir):
        if file.endswith('.py.done') \
        or file.endswith('.py.gone') \
        or file.endswith('.history'):
            number = file.split('_', 1)[0]
            data += ['%s %s' % (x.strip(), number) \
                     for x in open('migrations/' + file).readlines()]
    data.sort()
    if len(parameters) == 1 and parameters[0].isdigit():
        tail = -1 * int(parameters[0])
    else:
        tail = -20
    if tail < 0:
        print('... %d more' % (len(data) + tail))
        data = data[tail:]
    print('\n'.join(data))
    

@target
def upgrade(match, deps):

    if environment == 'safeproduction':
        raise PikeException('Safe production mode. Migration features are disabled.')

    global parameters
    assert len(parameters) == 1, 'which migration to upgrade?'
    mig = parameters[0]
    assert os.path.exists(mig), 'migration not found'
    assert os.path.basename(mig)[:-3] not in done_migrations(), 'migration already up'
    parameters = []
    _migrate(mig, 'up', {})
    if environment == 'development':
        require('cache>dicts')

@target
def downgrade(match, deps):

    if environment == 'safeproduction':
        raise PikeException('Safe production mode. Migration features are disabled.')

    global parameters
    assert len(parameters) == 1, 'which migration to downgrade?'
    mig = os.path.basename(parameters[0])
    assert os.path.exists(migcache_dir + '/' + mig), 'migration not found'
    assert mig[:-3] in done_migrations(), 'migration not up'
    parameters = []
    _migrate(migcache_dir + '/' + mig, 'down', {})
    if environment == 'development':
        require('cache>dicts')


# Fixture loading (initial setup-fixtures)

def active_cdr_db_pf(tenant):
    if '-S' in open('common.pf').read():
        connection_type = "tcp"
    else:
        connection_type = "local"

    args = ['-b', '-p', 'Syst/list_active_cdr_databases.p', '-param', connection_type]

    if not tenant == '':
        args.extend(['-pf', 'common_{}.pf'.format(tenant)])
    else:
        args.extend(['-pf', 'common.pf'])

    cdr_fetch = Popen(mpro + args, stdout=PIPE)
    dict = literal_eval(Popen('/bin/cat', stdin=cdr_fetch.stdout, stdout=PIPE).communicate()[0])

    if not tenant == '':
        uandp = userandpass(['-U', '{0}@{1}'.format(tenancies[tenant]['username'], tenancies[tenant]['domain']), '-P', tenancies[tenant]['password'] ])
        for db in dict:
            dict[db].extend(uandp)

    return dict

@target
def fixtures(*a):

    if environment == 'safeproduction':
        raise PikeException('Safe production mode. Fixture loading is not allowed.')

    tenantdict = {}
    tenant = None
    for tenant in tenancies:
        if tenancies[tenant]['tenanttype'] != 'Super':
            tenantdict[tenant] = {'tenant': tenant, 'pf': 'all_{}.pf'.format(tenant)}
        if tenancies[tenant]['tenanttype'] == 'Default':
            tenantdict['Default'] = {'tenant': tenant, 'pf': 'all_{}.pf'.format(tenant)}
    if tenant is None:
        tenantdict[''] = {'tenant': '', 'pf': 'all.pf' }

    for tenant in tenantdict:
        if not tenant == '' and not tenant == 'Default':
            print('Loading fixtures for tenant t{}...'.format(tenantdict[tenant]['tenant']))
            fixturedir = '{0}/db/progress/fixtures/{1}'.format(work_dir, tenantdict[tenant]['tenant'])
            try:
                os.makedirs(fixturedir)
            except OSError:
                if not os.path.isdir(fixturedir):
                    raise PikeException('Cannot use directory {}'.format(fixturedir))
        else:
            print('Loading fixtures...')
            fixturedir = '{0}/db/progress/fixtures'.format(work_dir)

        args = ['-pf', tenantdict[tenant]['pf'], '-b', '-p', 'gearbox/fixtures/load_fixtures.r',
                '-param', 'fix_dir={},bulk=yes'.format(fixturedir)]

        cdr_dict = {}

        for pp in cdr_databases:
            if not cdr_dict:
                cdr_dict = active_cdr_db_pf(tenantdict[tenant]['tenant'])
            args.extend(cdr_dict[pp])

        load_fixture = Popen(mpro + args, stdout=PIPE)
        call('/bin/cat', stdin=load_fixture.stdout)

# Tenant creation

@target
def tenanciescreate(*a):

    if environment != 'development':
        raise PikeException('Tenancy creation is only allowed in development mode.')

    if tenancies:
        cdr_dict = {}
        args = ['-pf', 'all.pf', '-b', '-p', 'multitenancy/create_tenant.r']
        for pp in cdr_databases:
            if not cdr_dict:
                cdr_dict = active_cdr_db_pf()
            args.extend(cdr_dict[pp])

        multitenancy = Popen(mpro + args, stdout=PIPE)
        call('/bin/cat', stdin=multitenancy.stdout)

@target
def dumpfixtures(*a):
    tenantdict = {}
    tenant = None
    for tenant in tenancies:
        if tenancies[tenant]['tenanttype'] != 'Super':
            tenantdict[tenant] = {'tenant': tenant, 'postfix': '_{}.pf'.format(tenant)}
        if tenancies[tenant]['tenanttype'] == 'Default':
            tenantdict['default'] = {'tenant': tenant, 'postfix': '_{}.pf'.format(tenant)}
    if tenant is None:
        tenantdict[''] = {'tenant': '', 'postfix': '.pf' }

    for tenant in tenantdict:
        if not tenant == '' and not tenant == 'default':
            print('Dumping fixtures for tenant t{}...'.format(tenantdict[tenant]['tenant']))
            fixturedir = '{0}/db/progress/fixtures/{1}'.format(work_dir, tenantdict[tenant]['tenant'])
            try:
                os.makedirs(fixturedir)
            except OSError:
                if not os.path.isdir(fixturedir):
                    raise PikeException('Cannot use directory {}'.format(fixturedir))
        else:
            print('Dumping fixtures...')
            fixturedir = '{0}/db/progress/fixtures'.format(work_dir)

        pf_entries = {}

        for pp in databases:
            pf_entries[pp] = ['-pf', '{0}{1}'.format(pp, tenantdict[tenant]['postfix'])]

        if environment != 'development':
            pf_entries.update(active_cdr_db_pf(tenantdict[tenant]['tenant']))

        for key in sorted(pf_entries):
            dump_fixture = Popen(mpro + pf_entries[key] + ['-b', '-p', 'gearbox/fixtures/dump_fixtures.p', '-param', '{}'.format(fixturedir)], stdout=PIPE)
            call('/bin/cat', stdin=dump_fixture.stdout)
