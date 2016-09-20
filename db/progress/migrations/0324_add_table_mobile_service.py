from gearbox.migrations import Migration

class AddTableService(Migration):

    database = "mobile"

    def up(self):
        t = self.table('Service', area="Sta_Data_128_2", label="Service", dump_name="service", desc='''
''')
        t.column('Service', 'character', format="x(8)", initial="", max_width=16, label="Service Group", column_label="Service", position=2, order=10, help="Code of Service (group)")
        t.column('SEName', 'character', format="x(40)", initial="", max_width=80, label="ServName", column_label="Name Of Service", position=3, order=20, help="Name of Service(group)")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=4, order=30, help="Memo of Service")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=5, order=40, help="Code Of Brand")
        t.index('Service', [['Brand'], ['Service']], area="Sta_Index_3", primary=True, unique=True)
        t.index('SEName', [['Brand'], ['SEName'], ['Service']], area="Sta_Index_3")

    def down(self):
        self.drop_table('Service')
