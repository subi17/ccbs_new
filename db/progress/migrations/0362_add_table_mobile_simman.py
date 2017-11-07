from gearbox.migrations import Migration

class AddTableSimMan(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SimMan', area="Sta_Data_128_2", dump_name="simman", desc='''
SIM manufacturer
''')
        t.column('Mancode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="Manufacturer", column_label="Manufacturer", position=2, order=10, help="Code of Manufacturer")
        t.column('ManName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Manufacturer Name", position=3, order=20, help="Name of Manufacturer")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=4, order=30, help="Code Of Brand")
        t.index('mancode', [['Brand'], ['Mancode']], area="Sta_Index_3", primary=True, unique=True)
        t.index('manname', [['Brand'], ['ManName'], ['Mancode']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('SimMan')
