from gearbox.migrations import Migration

class AddTableInvRunLog(Migration):

    database = "common"

    def up(self):
        t = self.table('InvRunLog', area="Sta_Data_128", label="Invoicing run", dump_name="invrunlo", desc="Invoicing run configuration")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User ID", column_label="User ID", position=2, order=30, help="Who started this run")
        t.column('InvCode', 'integer', format="99", initial="0", max_width=4, label="Invoice Code", column_label="Invoice Code", position=3, order=40, help="Code for week / day of a month for used invoice run (99)")
        t.column('State', 'integer', format="9", initial="0", max_width=4, label="State", column_label="State", position=4, order=50, help="State (0=not ran,1=running,2=ran)")
        t.column('Date', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=5, order=60, help="Date when ran")
        t.column('BillDurat', 'integer', format=">>>>>9", initial="0", max_width=4, label="Duration", column_label="Duration", position=6, order=70, help="Duration of run")
        t.column('InvQty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="AmtInv", column_label="AmtInv", position=7, order=80, help="How many invoices was created")
        t.column('InvAmt', 'decimal', format=">>>>>>>9.99", decimals=2, initial="0", max_width=17, label="ValInv", column_label="ValInv", position=8, order=90, help="Value of created invoices")
        t.column('Period', 'integer', format="999999", initial="0", max_width=4, label="Period", column_label="Period", position=9, order=100, help="Period when this run will be / was ran")
        t.column('StartStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time when this run was started", max_width=20, label="Started", column_label="Started", position=10, order=110, description="Time Stamp yyyymmdd.time (sec)")
        t.column('InvGroup', 'character', format="x(8)", initial="", max_width=16, label="InvGroup", column_label="InvGroup", position=11, order=120, help="Alphanumeric code for Invoicing Group")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=12, order=130, help="Code Of Brand")
        t.index('Date', [['Brand'], ['Date', 'DESC'], ['InvCode', 'DESC'], ['InvGroup', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('Period', [['Brand'], ['Period', 'DESC'], ['Date', 'DESC'], ['InvCode'], ['InvGroup', 'DESC']], area="Sta_Index_2")
        t.index('StartStamp', [['Brand'], ['Date', 'DESC'], ['StartStamp', 'DESC']], area="Sta_Index_2")
        t.index('State', [['Brand'], ['State']], area="Sta_Index_2")

    def down(self):
        self.drop_table('InvRunLog')
