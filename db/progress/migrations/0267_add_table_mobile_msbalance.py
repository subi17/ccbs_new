from gearbox.migrations import Migration

class AddMSBalance(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MSBalance', area='Sta_Data_128',
                       label='Mobsub Balance',
                       dump_name='msbalance',
                       desc='Subscription balances')
        t.column('MSSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='SubsID')
        t.column('BalType', 'character', initial='',
                 label='Balance Type',
                 column_label='Balance',
                 help='Balance type')
        t.column('Amount', 'decimal', decimals=5, format='->>>>>>>9.99', initial='0',
                 label='Balance',
                 help='Current balance')
        t.column('BalDate', 'date', format='99-99-99',
                 label='Balance Date',
                 column_label='Date',
                 help='Latest update to balance')
        t.column('CustNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer number')
        t.index('CustNum', ['CustNum', 'MSSeq', 'BalType'], area='Sta_Index_1')
        t.index('MsSeq', ['MSSeq', 'CustNum', 'BalType'], area='Sta_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MSBalance')

