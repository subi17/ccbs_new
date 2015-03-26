from gearbox.migrations import Migration

class AddCurrency(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Currency', area='Sta_Data_128',
                       label='Currency',
                       dump_name='currenc',
                       desc='Currency file')
        t.column('Currency', 'character', format='x(3)', initial='',
                 column_label='Currency',
                 help='Currency code')
        t.column('CurrName', 'character', format='x(20)', initial='',
                 label='Currency Name',
                 column_label='Currency Name',
                 help='Name of the currency')
        t.column('SubUnit', 'character', initial='',
                 column_label='SubUnit',
                 help='Subunit of currency (1/100)')
        t.column('SubName', 'character', format='x(12)', initial='',
                 column_label='SubName',
                 help='Name of subunit')
        t.index('Currency', ['Currency'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CurrName', ['CurrName', 'Currency'], area='Sta_Index_2',
                unique=True)
        t.index('subunit', ['SubUnit', 'Currency'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('Currency')

