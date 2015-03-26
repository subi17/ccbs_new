from gearbox.migrations import Migration

class AddExchange(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Exchange', area='Sta_Data_256',
                       label='Exchange',
                       dump_name='exchange',
                       desc='Exchange')
        t.column('ExCode', 'character', initial='',
                 label='Exchange',
                 column_label='Exchange',
                 help='Exchange\'s code')
        t.column('ExName', 'character', format='x(40)', initial='',
                 label='Exchange\'s name',
                 column_label='Exchange\'s name')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('AreaCode', 'character', format='x(4)', initial='',
                 label='Area Code',
                 column_label='Area Code')
        t.column('Ident', 'character', initial='',
                 column_label='Ident')
        t.column('ExNum', 'integer', format='>>>9', initial='0',
                 label='Ex-num',
                 column_label='Ex-num',
                 help='Exchange number')
        t.column('Local', 'logical', initial='no',
                 column_label='Local')
        t.column('Options', 'character', format='x(12)', initial='',
                 column_label='Options')
        t.index('ExCode', ['ExCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ExName', ['ExName', 'ExCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Exchange')

