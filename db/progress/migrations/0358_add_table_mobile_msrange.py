from gearbox.migrations import Migration

class AddTableMSRange(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MSRange', area="Sta_Data_256", dump_name="msrange", desc='''MSISDN Range
''')
        t.column('CLIFrom', 'character', format="x(12)", initial="", max_width=24, label="From", column_label="From", position=2, order=10, help="First MSISDN Number in Range")
        t.column('CLITo', 'character', format="x(12)", initial="", max_width=24, label="To", column_label="To", position=3, order=20, help="Last MSISDN Number in Range")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=4, order=30, help="Customer that owns this range")
        t.column('ReserveDate', 'date', format="99-99-99", max_width=4, label="ResDate", column_label="ResDate", position=5, order=40, help="Date when Range was Reserved")
        t.column('ExpireDate', 'date', format="99-99-99", max_width=4, label="ResEnds", column_label="ResEnds", position=6, order=50, help="Date When Reservation Expires if Not Used")
        t.column('SalesMan', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=7, order=60, help="Salesman's code")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=8, order=70, help="Code Of Brand")
        t.index('CLIFrom', [['Brand'], ['CLIFrom']], area="Sta_Index_3", primary=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['CLIFrom']], area="Sta_Index_3", unique=True)
        t.index('CustNum_s', [['CustNum'], ['CLIFrom']], area="Sta_Index_3", unique=True)
        t.index('SalesMan', [['Brand'], ['SalesMan'], ['CustNum']], area="Sta_Index_3")

    def down(self):
        self.drop_table('MSRange')
