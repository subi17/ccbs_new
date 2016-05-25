from gearbox.migrations import Migration

class AddTableOrderDelivery(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderDelivery', area="Sta_Data_32", table_trigger=[{'crc': '?', 'procedure': 'rd-orderdelivery.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-orderdelivery.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="orderdelivery")
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", help="Order sequence number", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('LOId', 'integer', format=">>9", initial="0", max_width=4, label="LOId", column_label="LOId", position=3, order=20, help="Logistic operator ID")
        t.column('LOTimeStamp', 'datetime', format="99-99-9999 HH:MM:SS", max_width=8, label="LOTimeStamp", column_label="LOTimeStamp", position=4, order=30, help="Date and time when the LO order status has changed")
        t.column('LOStatusId', 'integer', format=">>>9", initial="0", max_width=4, label="LOStatusId", column_label="LOStatusId", position=5, order=40, help="LO order status code")
        t.column('CourierId', 'integer', format=">>9", initial="0", help="Partner courier company code", max_width=4, label="CourierId", column_label="CourierId", position=6, order=50, description="Partner courier company code")
        t.column('CourierShippingId', 'character', format="x(20)", initial="", help="Courier shipment ID", max_width=40, label="CourierShippingId", column_label="CourierShippingId", position=7, order=60, description="Courier shipment ID - just in case someone must search on the courier web for extra delivery information about a specific order")
        t.column('IncidentInfoId', 'integer', format=">>>9", initial="0", max_width=4, label="IncidentInfoId", column_label="IncidentInfoId", position=8, order=70, description="Information code sent by courier when there is a transport incident during the delivery service")
        t.column('MeasuresInfoId', 'integer', format=">>>9", initial="0", max_width=4, label="MeasuresInfoId", column_label="MeasuresInfoId", position=9, order=80, description="Code of the measures taken by the courier company to solve a transport incident")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=10, order=90, help="Code Of Brand")
        t.index('OrderID', [['Brand'], ['OrderId'], ['LOTimeStamp', 'DESC']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('OrderDelivery')
