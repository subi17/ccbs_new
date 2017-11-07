# python Customer Care and Billing System (pCCBS)

## Overview

The repository contains a template project structure for open-edge based
applications in Qvantel.

When creating a new customer or new module, start off with these files
(remove this readme) and start adding source files.

## Details

The structure uses [pike](../../pike/browse) and defines any many
directories the targets that are commonly executed there:

 * database creation, migration and fixture loading in db/progress/store
 * code compilation and daemon starting in tms/ (or any other tagged dir)
 * test code execution in code_dir/test
 * dependency installation in srcpkgs
 * compilation and distribution

For these it depends on [gearbox](../../gearbox/browse), a set of tools which
implements all these features for Progress OpenEdge.
The Makefile.py files in CCBS only bind them all together with a
conventional data structure.

The makefiles figure out whether the current working directory is a
development checkout (cloned from the RCS) or a on production 
(unpacked distribution tarball) and handles especially the databases
accordingly differently.

## More features

Prefilled into the tools directory are some tools too small for gearbox:

 * provim: VIm bindings for editing .p source code: data dictionary lookups
...and in-editor syntax checks and compilation.
 * docgen: creates lookup-tables for provim and can export the tms
...menu hierarchy on a nice confluence page.

## First usage

For the makefiles to run two configuration files are needed.

#### Customization

When you run pike for the very first time, it will notice that etc/config.py
is missing and interactively ask you to fill in some information about this
project: (customer) name and required progress version.

Out of this, etc/config.py will be created and can be manually adapted later.
You will need to add the (openedge) database names here.

This file should be tracked by the version control system.

You will also need to setup database definitions for production environments
in db/progress/store/Makefile.py.  Read the comments in that file for help.

### Initialization

Even after etc/config.py exists, the working directory needs to be initialized
for your site (i.e. etc/site.py needs to be created).

This might happen completely automatically, as the script will investigate
the environment, find the progress directory in /opt/progress or /opt/dlc
and deduce the project directory from cwd.

One question will pop up when ports for the databases are not defined in
your service registry (/etc/services on POSIX).
If you don't know what this means it's safe to just press enter.

Initialization will descend into the database and srcpkg subdirectory and
create development databases and install the dependencies in srcpkgs.

## Targets in different directories

#### Project root

 * rundaemons
 * stopdaemons  (will proxy to each of the **modules** in etc/config.py)
 * dist (compiles all code and produces a tarball)

#### srcpkg

 * initialize (install all *name-version.tar* packages into ../tools)
 * build (called by root>dist to install all deps into the distribution)

Note that each tool receives a hidden **.version** file into it's install
directory under tools.
If the version of the tarball changes, *any* target called by pike will
issue a warning. In that case come here and run initialize = default.

#### support

 * default (starts an interactive tool that allows execution of scripts).

#### db

Proxies all commands into the subdirectory of each DBMS.
In practice that is only "progress" - the OpenEdge database.

#### db/progress/store

 * start, stop               (control database servers)
 * create + many file targets (create structure, database and connection files)
 * migrate, status, history  (migration framework control)
 * upgrade, downgrade        (run a given migration)
 * fixtures                  (load all fixtures in db/progress/fixtures)

For migrations note that the revision controlled migrations are expected in
db/progress/migrations.
The cache (to identify which migrations have been applied) is under db/progress/store/migrations.

If the environment is identified as production environment then the store
directory is removed and instead linked to the hosts database directory
as configured in the Makefile here.

A development environment will keep a (small scale) development db here.

#### tms (or any other module)

Code module directories are configured in etc/config.py and by default that is
a directory called tms.

 * test (proxies to the test subdirectory)
 * start, stop  (daemon control)
 * build (compiles all source files into the distribution)
 * hotfix (produce only the .pl file for hot-substitution in production)
 * compile, preprocess, xref (invoke the progress compiler)
 * clean  (delete all procore and compiled libraries)
 * run    (run the CUI script)
 * editor (run in interactive mode, i.e. "start the editor")
 * batch  (run a script given as parameter in non-interactive mode)

It is common to tweak runtime arguments configured here, e.g. run_extraargs.

#### tms/test

The test directory also has a db-subdirectory, whose Makefile.py points at
the main db/progress/store/Makefile.py.
It uses the same migrations but has it's own migration cache.

 * unit       (run all tests in unit/\*\*/\*.cls via gearbox.unit)
 * script     (run all \*\*/test\_\*.py in python)
 * functional (run functional/index.html via gearbox.functional)
 * consistency (run all consistency/\*.cls via gearbox.unit ON THE MAIN DB)
 * test     (run all of the above)
 * ci_test  (run all but consistency tests)

Parameters can be given to these targets to execute only given tests

## Author
Fabian Kreutz

