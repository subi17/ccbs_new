from gearbox.migrations import Migration

class AddTableOrderAction(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderAction', area="Sta_Data_128", label="Order Action", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-orderaction.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-orderaction.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="orderaction", desc="Order action")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brands")
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=3, order=20, description="Order sequence number")
        t.column('ItemType', 'character', format="x(16)", initial="", max_width=32, label="Item Type", column_label="Type", position=4, order=30, help="Item type")
        t.column('ItemKey', 'character', format="x(20)", initial="", max_width=40, label="Item Key", column_label="Key", position=5, order=40, help="Item key")
        t.column('ItemParam', 'character', format="x(20)", initial="", max_width=40, label="ItemParam", column_label="ItemParam", position=6, order=50, help="Extra parameters for Item")
        t.index('OrderId', [['Brand'], ['OrderId']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('OrderAction')
