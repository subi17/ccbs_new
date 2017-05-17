from gearbox.migrations import Migration

class AddTableOrderService(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderService', area="Sta_Data_32", dump_name="orderser")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('Service', 'character', format="x(12)", initial="", max_width=24, label="Service", column_label="Service", position=3, order=20)
        t.column('ServValue', 'character', format="x(6)", initial="", max_width=12, label="Value", column_label="Value", position=4, order=30)
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('OrderId', [['Brand'], ['OrderId']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('OrderService')
