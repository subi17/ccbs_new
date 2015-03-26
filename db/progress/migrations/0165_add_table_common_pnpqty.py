from gearbox.migrations import Migration

class AddPnpQty(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PnpQty', area='Sta_Data_256',
                       dump_name='pnpqty')
        t.column('CLI', 'character', format='x(12)', initial='',
                 column_label='CLI',
                 help='CLI number')
        t.column('Period', 'integer', format='999999', initial='0',
                 help='PNP Period')
        t.column('Qty', 'integer', format='>>>>>>9', initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Amount of call to this PNP CLI/Period')
        t.column('TotQty', 'integer', format='zzzzz9', initial='0',
                 column_label='TotQty',
                 help='Total Quantity of PNP ticket')
        t.index('Cli', ['CLI', 'Period'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('Period', [('Period', 'DESCENDING'), 'CLI'], area='Sta_Index_2')

    def down(self):
        self.drop_table('PnpQty')

