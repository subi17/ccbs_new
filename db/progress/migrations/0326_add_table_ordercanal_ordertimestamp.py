from gearbox.migrations import Migration

class AddOrderTimeStamp(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderTimeStamp', area='Sta_Data_32',
                       dump_name='ordertim')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('RowType', 'integer', format='>>9', initial='0',
                 column_label='RowType',
                 help='Order customer type')
        t.column('TimeStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='TimeStamp')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('OrderId', ['Brand', 'OrderId', 'RowType'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('OrderTimeStamp')

