from gearbox.migrations import Migration

class Addprinter(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('printer', area='Sta_Data_256')
        t.column('PrinterCode', 'character', format='X(12)', initial='',
                 label='Printer code',
                 help='Visual name of printer')
        t.column('port', 'character', format='X(30)', initial='',
                 label='Port',
                 help='Fysical port name')
        t.column('command', 'character', format='X(50)', initial='',
                 label='Command',
                 help='Output through command')
        t.column('printerType', 'character', initial='',
                 label='Printer type',
                 help='Printer type. Postscritp,PCL')
        t.index('main', ['PrinterCode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('printer')

