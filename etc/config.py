# Project configuration (should be under RC)
from os import environ as ENV

mpro           = ['%s/bin/mpro' % dlc,
                  '-pf', '%s/etc/pf/formats.pf' % (work_dir)]
appname        = 'yoigo'
appversion     = '0.1'
proversion     = '112'
modules        = ['tms', 'tmsrpc']
databases      = ['common', 'ordercanal', 'mobile', 'counter', 'star', 'mcdr', 'mcdrdtl', 'prepcdr', 'prepedr', 'roamcdr', 'fraudcdr', 'reratelog']
#rpcs           = {'atm': '/', 'dextra': '/tmsrpc_xfera', 'heat': '/tmsrpc_xfera', 'newton': '/fcgiproxy_xfera'}
#rpc_conf       = {'port': 4001, 'run_as': None}
rpcs           = {'newton': '/fcgiproxy_xfera'}
rpc_conf       = {'port': 3000, 'run_as': None}
wwwrealm       = 'yes'
ENV['DLC']     = dlc
ENV['display_banner'] = 'no'
