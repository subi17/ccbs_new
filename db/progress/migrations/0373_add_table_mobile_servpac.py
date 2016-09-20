from gearbox.migrations import Migration

class AddTableServPac(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ServPac', area="Sta_Data_128_2", label="SerPac", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-servpac.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-servpac.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="servpac", desc='''
''')
        t.column('ServPac', 'character', format="x(12)", initial="", max_width=24, label="Service Package", column_label="ServPack", position=2, order=10, help="Service Package Code")
        t.column('SPName', 'character', format="x(40)", initial="", max_width=80, label="Service Pack Name", column_label="Name", position=3, order=20, help="Name of Service Package")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=4, order=30, help="Memo of Service Package")
        t.column('FeeModel', 'character', format="x(12)", initial="", max_width=24, label="FeeModel", column_label="FeeModel", position=5, order=40, help="Possible Billing Event")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=6, order=50, help="Code Of Brand")
        t.column('ServiceLimit', 'character', format="x(16)", initial="", max_width=32, label="Service Limit", column_label="SLimit", position=8, order=60, help="Service limit group")
        t.index('ServPac', [['Brand'], ['ServPac']], area="Sta_Index_3", primary=True, unique=True)
        t.index('SPName', [['Brand'], ['SPName'], ['ServPac']], area="Sta_Index_3")

    def down(self):
        self.drop_table('ServPac')
