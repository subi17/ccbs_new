from gearbox.migrations import Migration

class AddTableOrderProductParam(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderProductParam', area="Sta_Data_64", multitenant="yes", dump_name="orderproductparam", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-orderproductparam.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-orderproductparam.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}])
        t.column('OrderProductID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Order Product ID", column_label="OrderProductID", position=2, order=10, description="Order product internal id")
        t.column('ParamName', 'character', format="x(20)", initial="", max_width=40, label="Param Name", column_label="ParamName", position=3, order=20)
        t.column('ValueType', 'character', format="x(8)", initial="", max_width=16, label="Value Type", column_label="ValueType", position=4, order=30)
        t.column('CharValue', 'character', format="x(20)", initial="", max_width=40, label="CharValue", column_label="CharValue", position=5, order=40)
        t.column('DateValue', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="DateValue", column_label="DateValue", position=6, order=50)
        t.column('DecValue', 'decimal', format="->>>>>>>9.99<<", decimals=5, initial="0", max_width=19, label="DecValue", column_label="DecValue", position=7, order=60)
        t.column('IntValue', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="IntValue", column_label="IntValue", position=8, order=70)
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=9, order=80, description="Order sequence number")
        t.index('OrderProductID', [['OrderProductID'], ['ParamName']], area="Sta_Index_64", primary=True, unique=True)

    def down(self):
        self.drop_table('OrderProductParam')
