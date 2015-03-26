from gearbox.migrations import Migration

class AddTMSPrinter(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSPrinter', area='Sta_Data_64',
                       label='Printers',
                       dump_name='tmsprint',
                       desc='Printers')
        t.column('PrinterId', 'character', format='x(24)', initial='',
                 label='Mnemonic',
                 column_label='Mnemonic',
                 help='Printer\'s logical name')
        t.column('Device', 'character', format='x(30)', initial='',
                 label='FysName',
                 column_label='FysName',
                 help='Printer\'s device name')
        t.column('PageLength', 'integer', format='ZZ9', initial='0',
                 label='PageSize',
                 column_label='PageSize',
                 help='Total length of page (default)')
        t.column('PageAvail', 'integer', format='ZZ9', initial='0',
                 label='Rows',
                 column_label='Rows',
                 help='Lines available on one page(default)')
        t.column('PageWidth', 'integer', format='ZZ9', initial='0',
                 column_label='PageWidht',
                 help='Default line width on printer ')
        t.column('LogCode', 'character', extent=5, format='X(100)', initial='',
                 label='Logical code',
                 help='Logical name for printer')
        t.column('DeviceCode', 'character', extent=5, format='X(100)', initial='',
                 label='Physical code',
                 help='Physical device')
        t.column('UseScript', 'logical', initial='no',
                 label='Use scripts',
                 column_label='Scripts',
                 help='Use Unix scripts in output command',
                 description='If true  then "output through" is used ')
        t.index('PrinterId', ['PrinterId'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TMSPrinter')

