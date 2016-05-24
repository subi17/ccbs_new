from gearbox.migrations import Migration

class AddTableOrderTimeStamp(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderTimeStamp', area="Sta_Data_32", dump_name="ordertim")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('RowType', 'integer', format=">>9", initial="0", max_width=4, label="RowType", column_label="RowType", position=3, order=20, help="Order customer type")
        t.column('TimeStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TimeStamp", column_label="TimeStamp", position=4, order=30)
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('OrderId', [['Brand'], ['OrderId'], ['RowType']], area="Sta_Index_1", primary=True)
        t.index('RowType', [['Brand'], ['RowType'], ['TimeStamp', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('OrderTimeStamp')
