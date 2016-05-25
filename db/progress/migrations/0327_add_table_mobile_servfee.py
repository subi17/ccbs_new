from gearbox.migrations import Migration

class AddTableServFee(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ServFee', area="Sta_Data_128", label="ServFee", dump_name="servfee", desc='''Fees from activating services
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('ServType', 'character', format="x(8)", initial="", help="Type of service", max_width=16, label="Service Type", column_label="Type", position=3, order=20, description="e.g. Report")
        t.column('ServKey', 'character', format="x(12)", initial="", help="Key value of service", max_width=24, label="Service Key", column_label="Key", position=4, order=30, description="e.g. report number")
        t.column('EventType', 'integer', format="9", initial="0", max_width=4, label="Event Type", column_label="Event", position=5, order=40, help="Event type")
        t.column('FeeModel', 'character', format="x(16)", initial="", max_width=32, label="Fee Model", column_label="FModel", position=6, order=50, help="Fees that are created when this event occurs")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From", position=7, order=60, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=8, order=70, help="Valid to")
        t.column('InterAct', 'logical', format="yes/no", initial="no", max_width=1, label="InterAct", column_label="InterAct", position=9, order=80, help="Ask before create new billing event")
        t.column('InvInfo', 'character', format="x(60)", initial="", max_width=120, label="Invoice Info", column_label="Info", position=10, order=90, help="Info to be written on invoice")
        t.index('ServType', [['Brand'], ['ServType'], ['ServKey'], ['EventType'], ['ToDate', 'DESC']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('ServFee')
