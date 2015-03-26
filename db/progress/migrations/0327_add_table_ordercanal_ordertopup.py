from gearbox.migrations import Migration

class AddOrderTopUp(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderTopUp', area='Sta_Data_32',
                       dump_name='ordertop')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('Amount', 'decimal', decimals=2, format='>>>>9.99', initial='0',
                 column_label='Amount')
        t.column('VatAmount', 'decimal', decimals=2, format='>>>>9.99', initial='0',
                 column_label='VatAmount')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('OrderId', ['Brand', 'OrderId'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('OrderTopUp')

