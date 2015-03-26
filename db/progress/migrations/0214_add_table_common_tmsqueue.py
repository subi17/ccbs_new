from gearbox.migrations import Migration

class AddTMSQueue(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSQueue', area='Dyn_Data_256',
                       label='Monthly Queue',
                       dump_name='tmsqueue',
                       desc='Monthly queue')
        t.column('CustNum', 'integer', mandatory=True, format='zzzzzz9', initial='0',
                 label=' Cust.nr',
                 column_label='CustNo',
                 help='Customer number, 1 ... 999999')
        t.column('Month', 'integer', format='999999', initial='0',
                 column_label='Month',
                 help='Year and month (YYYYMM)',
                 description='Year and month')
        t.column('Queued', 'decimal', decimals=2, format='->>>9.99', initial='0',
                 label='Value',
                 column_label='Value',
                 help='Queued value')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Brand', ['Brand', 'Month'], area='Dyn_Index_1',
                primary=True)

    def down(self):
        self.drop_table('TMSQueue')

