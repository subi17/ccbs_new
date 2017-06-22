# Project configuration (should be under RC)
from os import environ as ENV

mpro           = ['%s/bin/mpro' % dlc,
                  '-pf', '%s/etc/pf/formats.pf' % (work_dir),
                  '-T', '%s/var/tmp' % (work_dir)]

if 'environment' in globals() and environment != 'development':
    mpro.append('-q')

appname        = 'yoigo'
appversion     = '0.1'
proversion     = '11.2'
modules        = ['tms', 'tmsrpc']
databases      = ['common', 'ordercanal', 'mobile', 'counter', 'star', 'prepedr', 'fraudcdr', 'reratelog']
cdr_databases  = ['mcdr', 'mcdrdtl', 'prepcdr', 'roamcdr']
rpcs           = { 'cctool': 'topup', 'dextra': 'dextra', 'masmovil': 'dextra', 'heat': 'heat', 'mnp': 'mnp', 'newton': 'newton', 'selfservice': 'selfservice', 'topup': 'topup', 'viptool': 'topup', 'mnp_mock': 'mnp_mock'}
wwwrealm       = 'yes'
client_timezone = 'local'
server_timezone = 'local'
rpcversion_filename = 'version.txt'
tenancies      = { 'yoigo': { 'domain': 'dyoigo', 'username': 'yoigo', 'password': 'yoigo', 'tenanttype': 'Default' } }
ENV['display_banner'] = 'no'
