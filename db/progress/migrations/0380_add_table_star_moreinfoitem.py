from gearbox.migrations import Migration

class Addmoreinfoitem(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('moreinfoitem', area='Sta_Data_256',
                       label='More Info Item',
                       dump_name='moreitem')
        t.column('programcode', 'character', format='X(12)', initial='',
                 label='Programcode')
        t.column('number', 'integer', format='>9', initial='0',
                 label='Number',
                 help='Number for moreinfo')
        t.column('itemnumber', 'integer', format='>9', initial='0',
                 label='Item',
                 help='Item number')
        t.column('startprogramcode', 'character', format='X(12)', initial='',
                 label='Programcode',
                 help='Programcode for starting program')
        t.column('msdo', 'character', format='X(50)', initial='',
                 label='Main SDO',
                 help='Program with recursize directory')
        t.column('msdofields', 'character', format='X(50)', initial='',
                 label='Main SDO fields',
                 help='List of main logicalfieldnames')
        t.column('ssdo', 'character', format='X(50)', initial='',
                 label='Sub SDO',
                 help='Program with recursize directory')
        t.column('ssdofields', 'character', format='X(50)', initial='',
                 label='Sub SDO fields',
                 help='List of main logicalfieldnames')
        t.column('paramterstr', 'character', format='X(50)', initial='',
                 label='Parameter',
                 help='Special parameter to subprogram..')
        t.column('accelerator', 'character', format='X(20)', initial='',
                 label='Accelerator',
                 help='Accelerator key')
        t.index('itemnumber', ['programcode', 'number', 'itemnumber'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('msdo', ['msdo'], area='Sta_Index_2')
        t.index('ssdo', ['ssdo'], area='Sta_Index_2')
        t.index('startprogramcode', ['startprogramcode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('moreinfoitem')

