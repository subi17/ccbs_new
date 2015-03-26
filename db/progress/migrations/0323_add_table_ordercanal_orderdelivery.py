from gearbox.migrations import Migration

class AddOrderDelivery(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderDelivery', area='Sta_Data_32',
                       dump_name='orderdelivery')
        t.column('OrderId', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 help='Order sequence number',
                 description='Order sequence number')
        t.column('LOId', 'integer', format='>>9', initial='0',
                 column_label='LOId',
                 help='Logistic operator ID')
        t.column('LOTimeStamp', 'datetime', format='99-99-9999 HH:MM:SS',
                 column_label='LOTimeStamp',
                 help='Date and time when the LO order status has changed')
        t.column('LOStatusId', 'integer', format='>>>9', initial='0',
                 column_label='LOStatusId',
                 help='LO order status code')
        t.column('CourierId', 'integer', format='>>9', initial='0',
                 column_label='CourierId',
                 help='Partner courier company code',
                 description='Partner courier company code')
        t.column('CourierShippingId', 'character', format='x(20)', initial='',
                 column_label='CourierShippingId',
                 help='Courier shipment ID',
                 description='Courier shipment ID - just in case someone must search on the courier web for extra delivery information about a specific order')
        t.column('IncidentInfoId', 'integer', format='>>>9', initial='0',
                 column_label='IncidentInfoId',
                 description='Information code sent by courier when there is a transport incident during the delivery service')
        t.column('MeasuresInfoId', 'integer', format='>>>9', initial='0',
                 column_label='MeasuresInfoId',
                 description='Code of the measures taken by the courier company to solve a transport incident')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('OrderID', ['Brand', 'OrderId', ('LOTimeStamp', 'DESCENDING')], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('OrderDelivery')

