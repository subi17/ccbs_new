#!/usr/bin/python

import xmlrpclib
from confluence_doc_conf_selfservice import applications
import os
import re
import sys

sys.path.insert(0, '/apps/xfera/tmsrpc/install/share/fcgi_agent/xmlrpc/')
from help import DocFile

def get_docs(appname):
    allfiles = [fn for fn in os.listdir(os.path.join(appname, 'rpcmethods/external/selfservice'))
                   if re.search(r'\.p$', fn)]
    allfiles.sort()
    return '\n'.join([DocFile(os.path.join(appname, 'rpcmethods/external/selfservice', fn)).to_confluence() \
                      for fn in allfiles])

def main():
    auth = sys.argv[1:3]
    if len(auth) != 2:
        sys.stderr.write("Authentication for luna needed.\n")
        sys.stderr.write("Syntax: %s username password\n" % sys.argv[0])
        sys.exit(1)
    
    for appname, values in applications.items():
        print "** Application %s" % appname
        newtext = '\n{toc}\n' + get_docs(appname)
        for url, pagename, func in values:
            print " * Server %s" % url
            if isinstance(pagename, basestring):
                if pagename.count('|') < 1:
                    print 'Format SPACE|TITLE for page names, not %s' % pagename
                    continue
                page = pagename.split('|', 1)
            elif isinstance(pagename, int):
                page = [pagename]
            else:
                print 'Invalid page identification: ', str(pagename)
                continue
            server = xmlrpclib.ServerProxy(url + '/rpc/xmlrpc')
            session = server.confluence2.login(*auth)
            try:
                page = server.confluence2.getPage(session, *page)
            except xmlrpclib.Fault, e:
                print e.faultString
                continue
            if func is None:
                newpagetext = newtext
            elif callable(func):
                try:
                    newpagetext = func(page['content'], newtext)
                except StandardError, e:
                    print e.message
                    continue
            else:
                print 'Invalid datatype for edit function: ' + str(type(func))
                continue
            if page['content'] != newpagetext:
                page['content'] = newpagetext
                storedPage = server.confluence1.storePage(session, page)
                print "   Done"
            else:
                print "   No changes"
            server.confluence2.logout(session)

if __name__ == '__main__':
    main()
