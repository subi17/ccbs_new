from gearbox.migrations import Migration

class AddInterest(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Interest', area='Sta_Data_256',
                       label='Overtime Interests Percents',
                       dump_name='interest',
                       desc='Overtime interests for different time periods\
')
        t.column('ValidFrom', 'date', mandatory=True, format='99-99-99',
                 label='Valid',
                 column_label='Valid',
                 help='Date from which this interest is valid')
        t.column('IntPerc', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='Interest %',
                 column_label='Interest %',
                 help='Amount of interest (%)')
        t.column('Memo', 'character', format='x(40)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('IntType', 'integer', format='9', initial='1',
                 label='Interest type',
                 column_label='IntType',
                 help='1 = fixed interest, 2 = in addition to euribor')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('IntPerc', ['Brand', 'IntPerc', 'ValidFrom'], area='Sta_Index_2')
        t.index('ValidFrom', ['Brand', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Interest')

