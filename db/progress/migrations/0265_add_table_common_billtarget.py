from gearbox.migrations import Migration

class AddTableBillTarget(Migration):

    database = "common"

    def up(self):
        t = self.table('BillTarget', area="Sta_Data_256", label="Billing Target", dump_name="billtarg", desc="Customer's Billing Target")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer Number")
        t.column('BillTarget', 'integer', format=">9", initial="0", max_width=4, label="Billing Target", column_label="BT", position=3, order=20, help="No. for customer's billing target")
        t.column('DiscPlan', 'character', format="x(12)", initial="", max_width=24, label="Discount Plan", column_label="Discount Plan", position=9, order=80, help="Code of a Discount Plan")
        t.column('RatePlan', 'character', format="x(12)", initial="", max_width=24, label="Rating Plan", column_label="RatePlan", position=15, order=130, help="Rating plan code")
        t.index('CustNum', [['CustNum'], ['BillTarget']], area="Sta_Index_1", primary=True, unique=True)
        t.index('RatePlan', [['RatePlan'], ['CustNum']], area="Sta_Index_1")

    def down(self):
        self.drop_table('BillTarget')
