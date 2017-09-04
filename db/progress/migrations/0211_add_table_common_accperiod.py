from gearbox.migrations import Migration

class AddTableAccPeriod(Migration):

    database = "common"

    def up(self):
        t = self.table('AccPeriod', area="Sta_Data_256", dump_name="accperio")
        t.column('Period', 'integer', format="999999", initial="0", max_width=4, label="Period", position=2, order=10, help="Accounting period")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From", position=3, order=20, help="Beginning of period")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=4, order=30, help="End of period")
        t.column('PerLocked', 'logical', format="Locked/Unlocked", initial="no", max_width=1, label="Locked", position=5, order=40, help="If period is locked then no events can be posted to it.")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('Period', [['Brand'], ['Period']], area="Sta_Index_2", primary=True, unique=True)
        t.index('FromDate', [['Brand'], ['FromDate']], area="Sta_Index_2", unique=True)
        t.index('ToDate', [['Brand'], ['ToDate']], area="Sta_Index_2")

    def down(self):
        self.drop_table('AccPeriod')
