from gearbox.migrations import Migration

class AddTableOrderTopUp(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderTopUp', area="Sta_Data_32", dump_name="ordertop")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('Amount', 'decimal', format=">>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", column_label="Amount", position=3, order=20)
        t.column('VatAmount', 'decimal', format=">>>>9.99", decimals=2, initial="0", max_width=17, label="VatAmount", column_label="VatAmount", position=5, order=40)
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('OrderId', [['Brand'], ['OrderId']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('OrderTopUp')
