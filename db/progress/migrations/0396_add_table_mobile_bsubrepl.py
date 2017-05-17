from gearbox.migrations import Migration

class AddTableBSubRepl(Migration):

    database = "mobile"

    def up(self):
        t = self.table('BSubRepl', area="Sta_Data_256", label="Replacement String For B-sub nos", dump_name="bsrepl")
        t.column('BSubValue', 'character', format="x(8)", initial="", max_width=16, label="Br-Type", column_label="Br-Type", position=2, order=10, help="BSub Number Replacement Type")
        t.column('BSubRepl', 'character', format="x(16)", initial="", max_width=32, label="Br-Key", column_label="Br-Key", position=3, order=20, help="B-Sub Number")
        t.column('BRName', 'character', format="x(30)", initial="", max_width=60, label="Br-Name", column_label="Br-Name", position=4, order=30, help="Text Which Replaces The Number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=5, order=40, help="Code Of Brand")
        t.index('BRName', [['Brand'], ['BSubValue'], ['BSubRepl', 'ABBREVIATED']], area="Sta_Index_3", primary=True)
        t.index('BSubValue', [['Brand'], ['BSubValue'], ['BSubRepl', 'ABBREVIATED']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('BSubRepl')
