from gearbox.migrations import Migration

class AddstarParam(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('starParam', area='Sta_Data_256',
                       dump_name='starpara')
        t.column('groupCode', 'character', format='X(20)', initial='',
                 label='Group',
                 help='Group code')
        t.column('paramCode', 'character', format='X(20)', initial='',
                 label='Code')
        t.column('c_value', 'character', extent=10, format='X(50)', initial='',
                 label='Character',
                 help='Character values')
        t.column('i_value', 'integer', extent=10, format='>>>>>>>>9', initial='0',
                 label='Integer',
                 help='Intger values')
        t.column('de_value', 'decimal', extent=10, decimals=9, format='->>>>>>>>>>>9.9<<<<<<<<', initial='0',
                 label='Decimal')
        t.column('da_value', 'date', extent=10, format='99.99.99',
                 label='Date',
                 help='Date values')
        t.column('l_value', 'logical', extent=10, initial='no',
                 label='Logical',
                 help='Locigal values')
        t.column('memo', 'character', format='X(50)', initial='',
                 label='Memo',
                 help='Memo of parameter')
        t.index('paramCode', ['groupCode', 'paramCode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('starParam')

