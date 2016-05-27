from gearbox.migrations import Migration

class AddTableBDestConfItem(Migration):

    database = "common"

    def up(self):
        t = self.table('BDestConfItem', area="Sta_Data_128", label="BDest Configuration Item", dump_name="bdestconfitem", desc="BDest configuration item")
        t.column('BDCGroup', 'character', format="x(12)", initial="", max_width=24, label="Config Group", column_label="Group", position=2, order=20, help="Configuration group ID")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=3, order=10, help="Code of brand")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=4, order=50, help="Valid from")
        t.column('BDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="B-nbr Destination", column_label="BDest", position=5, order=30, help="B-number destination")
        t.column('RateCCN', 'integer', format=">>9", initial="0", max_width=4, label="Rating CCN", column_label="RateCCN", position=6, order=40, help="Rating CCN")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=7, order=60, help="Last effective day")
        t.column('BDCItemID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Item ID", column_label="ID", position=8, order=70, help="Unique item ID")
        t.index('BDCGroup', [['Brand'], ['BDCGroup']], area="Sta_Index_2", primary=True)
        t.index('BDCItemID', [['BDCItemID']], area="Sta_Index_2", unique=True)
        t.index('BDest', [['Brand'], ['BDest'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BDestConfItem')
