from gearbox.migrations import Migration

class AddTableASubType(Migration):

    database = "common"

    def up(self):
        t = self.table('ASubType', area="Sta_Data_256", label="ASub Type", dump_name="asubtype", desc='''Types of a-subscriber (mobile, fixed, voip)
''')
        t.column('ASubName', 'character', format="x(30)", initial="", max_width=60, label="Name", position=3, order=20, help="Description of the a-subscriber type")
        t.column('ASubType', 'integer', format="9", initial="0", max_width=4, label="A-Type", column_label="A-Type", position=4, order=10, help="ASUB (Dialler) type")
        t.index('ASubType', [['ASubType']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('ASubType')
