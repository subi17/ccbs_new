from gearbox.migrations import Migration

class AddTablePreselErr(Migration):

    database = "common"

    def up(self):
        t = self.table('PreselErr', area="Sta_Data_256", label="Preselection Error Codes", dump_name="preseler", desc="Preselection transaction error codes")
        t.column('PSError', 'integer', format="z9", initial="0", max_width=4, label="Error Code", column_label="Error Code", position=2, order=10, help="Error Code")
        t.column('PSEName', 'character', format="x(40)", initial="", max_width=80, label="Error Name", column_label="Error Name", position=3, order=20, help="Name of error")
        t.index('PSError', [['PSError']], area="Sta_Index_2", primary=True, unique=True)
        t.index('PSEName', [['PSEName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PreselErr')
