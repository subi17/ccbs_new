from gearbox.migrations import Migration

class AddCustCount(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CustCount', area='Sta_Data_256',
                       label='Customer counters',
                       dump_name='custcoun',
                       desc='Customer counters\
\
')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('Unbilled', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 help='Amount of customer\'s unbilled events')
        t.index('CustNum', ['CustNum'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CustCount')

