# Project configuration (should be under RC)
from os import environ as ENV

mpro           = ['%s/bin/mpro' % dlc,
                  '-pf', '%s/etc/pf/formats.pf' % (work_dir)]
appname        = 'yoigo'
appversion     = '0.1'
proversion     = '112'
modules        = ['tms', 'tmsrpc']
databases      = ['common', 'ordercanal', 'mobile', 'counter', 'star', 'prepedr', 'fraudcdr', 'reratelog']
cdr_databases  = ['mcdr', 'mcdrdtl', 'prepcdr', 'roamcdr']
rpcs           = { 'cctool': 'topup', 'dextra': 'dextra', 'heat': 'heat', 'mnp': 'mnp', 'newton': 'newton', 'selfservice': 'selfservice', 'topup': 'topup', 'viptool': 'topup' }
wwwrealm       = 'yes'
client_timezone = 'local'
server_timezone = 'local'
ENV['DLC']     = dlc
ENV['display_banner'] = 'no'
