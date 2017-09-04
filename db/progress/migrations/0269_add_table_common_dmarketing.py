from gearbox.migrations import Migration

class AddTableDMarketing(Migration):

    database = "common"

    def up(self):
        t = self.table('DMarketing', area="Sta_Data_256", label="Direct marketing", dump_name="dmarketi", desc="Direct marketing codes")
        t.column('DirMark', 'character', format="x(8)", initial="", max_width=16, label="Direct Marketing", column_label="Dir.Market", position=2, order=10, help="Direct marketing code")
        t.column('DirMarkName', 'character', format="x(30)", initial="", max_width=60, label="DM Name", column_label="DMName", position=3, order=20, help="Direct marketing name")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=4, order=30, help="Code Of Brand")
        t.index('DirMark', [['Brand'], ['DirMark']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DirMarkName', [['Brand'], ['DirMarkName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DMarketing')
