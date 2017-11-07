from gearbox.migrations import Migration

class AddTableCLISer(Migration):

    database = "star"

    def up(self):
        t = self.table('CLISer', area="Sta_Data_256", label="CLISer", dump_name="cliser", desc="CLI series records, from - to")
        t.column('CLIFrom', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="A-subFrom", column_label="FirstASub", position=2, order=10, help="First A-sub no in a series")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=3, order=30, help="Customer no. (owner) of an A-sub no.")
        t.column('CLITo', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="A-subTill", column_label="LastAsub", position=4, order=20, help="Last A-sub. no in a number series")
        t.column('Memo', 'character', format="x(30)", initial="", max_width=60, label="Memo", column_label="Memo", position=5, order=40, help="Text that will be printed on calls / a-number report")
        t.column('Secr', 'logical', format="Yes/No", initial="no", max_width=1, label="Secret", column_label="Secret", position=6, order=50, help="Is this a SECRET number series (Y/N)")
        t.column('SerNum', 'integer', mandatory=True, format="zzzzzzz9", initial="0", max_width=4, label="SerNum", column_label="SerNum", position=7, order=60, help="Consecutive number (sequence) of series")
        t.column('BillTarget', 'integer', format=">9", initial="0", max_width=4, label="Billing Target", column_label="Bill.Targ", position=8, order=210, help="Customer's billing target")
        t.index('CLISer', [['CLIFrom'], ['CLITo'], ['CustNum']], area="Sta_Index_2", primary=True)
        t.index('BillTarget', [['CustNum'], ['BillTarget']], area="Sta_Index_2")
        t.index('CustNum', [['CustNum'], ['CLIFrom']], area="Sta_Index_2")
        t.index('SerNum', [['SerNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('CLISer')
