from gearbox.migrations import Migration

class AddTableOrderProduct(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderProduct', area="Sta_Data_64", multitenant="yes", dump_name="orderproduct", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-orderproduct.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-orderproduct.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}])
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('OrderProductID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderProductID", column_label="OrderProductID", position=3, order=20, description="Order product internal id")
        t.column('ParentID', 'integer', format=">>9", initial="0", max_width=4, label="Parent ID", column_label="ParentID", position=4, order=30)
        t.column('ProductID', 'character', format="x(20)", initial="", max_width=40, label="Product ID", column_label="ProductID", position=5, order=40, help="Product Catalog product ID")
        t.column('ProductOfferingID', 'character', format="x(20)", initial="", max_width=40, label="Product Offering ID", column_label="ProductOfferingID", position=6, order=50, help="Product Catalog product offering ID")
        t.column('ActionType', 'character', format="x(15)", initial="", max_width=30, label="Action Type", column_label="ActionType", position=7, order=60)
        t.index('OrderProductID', [['OrderProductID']], area="Sta_Index_64", primary=True, unique=True)
        t.index('OrderID', [['OrderId']], area="Sta_Index_64")

    def down(self):
        self.drop_table('OrderProduct')
