from gearbox.migrations import Migration

class AddOperator(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Operator', area='Sta_Data_128',
                       label='Operators',
                       dump_name='operator',
                       desc='Operators')
        t.column('Operator', 'character', mandatory=True, initial='',
                 label='OpCode',
                 column_label='OpCode',
                 help='Operator\'s code, 1 - 8 characters')
        t.column('OperName', 'character', format='x(40)', initial='',
                 column_label='OperName',
                 help='Operator\'s name')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memotext')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Operators customer number (used for collecting indirect calls)')
        t.column('Prefix', 'character', initial='',
                 label='Pref',
                 column_label='Pref',
                 help='Prefix used with number portability')
        t.index('Operator', ['Operator', 'OperName'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('OperName', ['OperName', 'Operator'], area='Sta_Index_2',
                unique=True)
        t.index('Prefix', ['Prefix', 'Operator'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('Operator')

