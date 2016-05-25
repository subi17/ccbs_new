from gearbox.migrations import Migration

class AddTableSIMStat(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SIMStat', area="Sta_Data_128", dump_name="simstat", desc='''SIM status codes
''')
        t.column('SimStat', 'integer', format="z9", initial="0", max_width=4, label="Sim Status", column_label="Status", position=2, order=10, help="SIM Status Code")
        t.column('SSName', 'character', format="x(40)", initial="", max_width=80, label="Name", column_label="Name", position=3, order=20, help="Name (description) of status")
        t.index('SimStat', [['SimStat']], area="Sta_Index_3", primary=True, unique=True)
        t.index('SSName', [['SSName'], ['SimStat']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('SIMStat')
