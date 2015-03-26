from gearbox.migrations import Migration

class AddHiUsageLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('HiUsageLimit', area='Sta_Data_256',
                       dump_name='hiusagel')
        t.column('Category', 'character', format='x(4)', initial='',
                 label='Cat',
                 column_label='Cat',
                 help='Category code')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='ProdCd',
                 column_label='ProdCd',
                 help='Product code, max 16 characters')
        t.column('Limit', 'integer', format='>,>>>,>>9', initial='0',
                 column_label='Limit',
                 help='Highusage limit',
                 description='Highusage limit')
        t.index('BillCode', ['BillCode', ('Limit', 'DESCENDING')], area='Sta_Index_2')
        t.index('Category', ['Category', 'BillCode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('HiUsageLimit')

