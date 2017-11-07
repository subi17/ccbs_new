from gearbox.migrations import Migration

class AddTableCustPNPSer(Migration):

    database = "common"

    def up(self):
        t = self.table('CustPNPSer', area="Sta_Data_256", label="Customer's PNP Numbers", dump_name="custpnp", desc="A-subscribers private b-number series")
        t.column('FromBDest', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="B-sub FROM", column_label="B-sub FROM", position=2, order=10, help="B-number series lower limit")
        t.column('CustNum', 'integer', format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=3, order=30, help="A-subscribers customer number")
        t.column('ToBDest', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="B-sub TO", column_label="B-sub TO", position=4, order=20, help="B-number series' upper limit")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('b-nr', [['Brand'], ['FromBDest'], ['ToBDest'], ['CustNum']], area="Sta_Index_2", primary=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['FromBDest']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum'], ['FromBDest']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustPNPSer')
