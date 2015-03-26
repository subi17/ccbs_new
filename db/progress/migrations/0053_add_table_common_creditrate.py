from gearbox.migrations import Migration

class AddCreditRate(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CreditRate', area='Sta_Data_32',
                       label='Credit Rating',
                       dump_name='creditra',
                       desc='Credit rating')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('PersonId', 'character', format='x(11)', initial='',
                 column_label='PersonId',
                 help='Personal Number',
                 description='Personal Number')
        t.column('CRReply', 'character', initial='',
                 label='Credit Rating Reply',
                 column_label='Reply',
                 help='Reply to credit rating request')
        t.column('Handler', 'character', format='x(20)', initial='',
                 help='User id of the credit rating request handler')
        t.column('CrStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Creation timestamp',
                 description='Creation timestamp')
        t.index('OrderID', ['OrderId'], area='Sta_Index_2',
                primary=True)
        t.index('PersonId', ['PersonId', ('CrStamp', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('CreditRate')

