from gearbox.migrations import Migration

class AddOrderService(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderService', area='Sta_Data_32',
                       dump_name='orderser')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('Service', 'character', format='x(12)', initial='',
                 column_label='Service')
        t.column('ServValue', 'character', format='x(6)', initial='',
                 label='Value',
                 column_label='Value')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('OrderId', ['Brand', 'OrderId'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('OrderService')

