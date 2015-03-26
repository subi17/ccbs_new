from gearbox.migrations import Migration

class AddCDRError(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CDRError', area='Sta_Data_256',
                       label='Error codes for Rating',
                       dump_name='cdrerror',
                       desc='CDR error numbers')
        t.column('CDRError', 'integer', format='zzz9', initial='0',
                 label='Error Code',
                 column_label='Error Code',
                 help='Error Code 1 ... 9999')
        t.column('CEName', 'character', format='x(60)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 help='Explanation (name) of an Error Code')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo of CDR Rating Error Code')
        t.index('CDRError', ['CDRError'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CEName', ['CEName', 'CDRError'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('CDRError')

