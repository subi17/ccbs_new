from gearbox.migrations import Migration

class AddTableBDestTrans(Migration):

    database = "common"

    def up(self):
        t = self.table('BDestTrans', area="Sta_Data_128", label="BDestTrans", dump_name="bdesttrans", desc="B-Destination short number translations")
        t.column('BDest', 'character', format="x(7)", initial="", max_width=14, label="BDest", column_label="BDest", position=2, order=10)
        t.column('RatingZone', 'character', format="x(2)", initial="", max_width=4, label="RatingZone", column_label="RatingZone", position=3, order=30)
        t.column('TranslateNumber', 'character', format="x(12)", initial="", max_width=24, label="TranslateNumber", column_label="TranslateNumber", position=4, order=40)
        t.column('BdestID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="BDestination ID", column_label="BDest ID", position=5, order=50, help="Unique ID")
        t.column('ToDate', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid To", column_label="ToDate", position=6, order=60, help="Last effective day")
        t.column('FromDate', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid From", column_label="FromDate", position=7, order=70, help="First effective day")
        t.index('BDestTrans', [['BdestID'], ['TranslateNumber'], ['ToDate', 'DESC']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('BDestTrans')
