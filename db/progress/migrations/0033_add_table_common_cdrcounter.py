from gearbox.migrations import Migration

class AddCDRCounter(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CDRCounter', area='Dyn_Data_128',
                       label='CDR Counter',
                       dump_name='cdrcount',
                       desc='Counters for CDR import')
        t.column('CDRType', 'integer', format='>>>>9', initial='0',
                 label='CDR Type',
                 column_label='Type',
                 help='CDR type')
        t.column('ImportDate', 'date', format='99-99-99',
                 label='Import Date',
                 column_label='Import',
                 help='Import date')
        t.column('Qty', 'integer', format='>>>>>>>9', initial='0',
                 label='Quantity',
                 column_label='Qty')
        t.column('Duration', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='Durat')
        t.column('Amount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 column_label='Amt')
        t.index('CDRType', ['CDRType', ('ImportDate', 'DESCENDING')], area='Dyn_Index_1')
        t.index('ImportDate', [('ImportDate', 'DESCENDING'), 'CDRType'], area='Dyn_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CDRCounter')

