from gearbox.migrations import Migration

class AddTableServEl(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ServEl', area="Sta_Data_256", label="Servel", dump_name="servel", desc='''Service elements (service component group)

''')
        t.column('ServPac', 'character', format="x(12)", initial="", max_width=24, label="Service Package", column_label="ServPac", position=2, order=10, help="Code of ServPack")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Component", position=3, order=20, help="Code of Service Component (a single service)")
        t.column('SeValue', 'integer', format=">>>9", initial="0", max_width=4, label="Value", column_label="Value", position=4, order=30, help="Default Value for Service Parameter")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=5, order=40, help="Code Of Brand")
        t.index('ServPac', [['Brand'], ['ServPac'], ['ServCom']], area="Sta_Index_3", primary=True, unique=True)
        t.index('ServCom', [['Brand'], ['ServCom'], ['ServPac']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('ServEl')
