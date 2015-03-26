from gearbox.migrations import Migration

class AddOrderAction(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderAction', area='Sta_Data_128',
                       label='Order Action',
                       dump_name='orderaction',
                       desc='Order action')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brands')
        t.column('OrderId', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('ItemType', 'character', format='x(16)', initial='',
                 label='Item Type',
                 column_label='Type',
                 help='Item type')
        t.column('ItemKey', 'character', format='x(20)', initial='',
                 label='Item Key',
                 column_label='Key',
                 help='Item key')
        t.index('OrderId', ['Brand', 'OrderId'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('OrderAction')

