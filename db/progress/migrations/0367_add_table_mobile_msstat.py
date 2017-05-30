from gearbox.migrations import Migration

class AddTableMSStat(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MSStat', area="Sta_Data_256", dump_name="msstat", desc='''MSISDN Status
''')
        t.column('StatusCode', 'integer', format="z9", initial="0", max_width=4, label="Status", column_label="St", position=2, order=10, help="Status Code")
        t.column('StatusName', 'character', format="x(40)", initial="", max_width=80, label="Status", column_label="Status", position=3, order=20, help="MSISDN Status")
        t.index('StatusCode', [['StatusCode']], area="Sta_Index_3", primary=True, unique=True)
        t.index('StatusName', [['StatusName'], ['StatusCode']], area="Sta_Index_3")

    def down(self):
        self.drop_table('MSStat')
