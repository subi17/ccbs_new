from gearbox.migrations import Migration

class AddMobError(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MobError', area='Sta_Data_128',
                       label='Error codes for Mobile Rating',
                       dump_name='mrerr',
                       desc='\
')
        t.column('MobError', 'integer', format='zzz9', initial='0',
                 label='Error Code',
                 column_label='Error Code',
                 help='Error Code 1 ... 9999')
        t.column('MEName', 'character', format='x(60)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 help='Explanation (name) of an Error Code')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo of Mobile Rating Error Code')
        t.index('MEName', ['MEName', 'MobError'], area='Sta_Index_3',
                unique=True)
        t.index('MobError', ['MobError'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MobError')

