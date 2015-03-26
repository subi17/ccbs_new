from gearbox.migrations import Migration

class AddCurRate(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CurRate', area='Sta_Data_256',
                       label='Exchange Rates',
                       dump_name='currate',
                       desc='Currency exchange rates')
        t.column('Currency', 'character', format='x(5)', initial='',
                 label='Code',
                 column_label='Code',
                 help='Currency code')
        t.column('ExchRate', 'decimal', decimals=6, format='>>9.999999', initial='0',
                 label='Rate',
                 column_label='Rate',
                 help='Currency rate')
        t.column('RateDate', 'date', format='99-99-99',
                 label='Date',
                 column_label='Date',
                 help='Date when updated')
        t.index('Currency', ['Currency', ('RateDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('RateDate', [('RateDate', 'DESCENDING'), 'Currency'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CurRate')

