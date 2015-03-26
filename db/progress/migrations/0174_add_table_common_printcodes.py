from gearbox.migrations import Migration

class AddPrintCodes(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PrintCodes', area='Sta_Data_32',
                       label='Printer Control Codes',
                       dump_name='printcod',
                       desc='Printer control codes')
        t.column('Effect', 'character', format='x(1)', initial='',
                 label='Code',
                 column_label='Code',
                 help='Code of a control character string')
        t.column('PrinterId', 'character', format='x(24)', initial='',
                 label='Printer',
                 column_label='Printer',
                 help='Printer\'s logical name')
        t.column('EffOn', 'character', extent=2, format='x(65)', initial='',
                 label='Begin',
                 column_label='Begin',
                 help='Print\'s start code')
        t.column('EffOff', 'character', extent=2, format='x(65)', initial='',
                 label='End',
                 column_label='End',
                 help='Terminating control code sequence')
        t.column('EffName', 'character', format='x(30)', initial='',
                 label='FX\'s name',
                 column_label='FX\'s name',
                 help='Descriptive name for a control code')
        t.column('PageWidth', 'integer', format='ZZ9', initial='0',
                 label='PageWe',
                 column_label='PageWe',
                 help='Maximum no. of characters per line on printer')
        t.column('PageLength', 'integer', format='ZZ9', initial='0',
                 column_label='PageLength',
                 help='Total lines per page')
        t.column('AvailLines', 'integer', format='ZZ9', initial='0',
                 label='Rows',
                 column_label='Rows',
                 help='Number of lines available on page')
        t.index('Effect', ['Effect', 'PrinterId'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('EffName', ['PrinterId', 'EffName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('PrintCodes')

