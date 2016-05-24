from gearbox.migrations import Migration

class AddTableCampStat(Migration):

    database = "common"

    def up(self):
        t = self.table('CampStat', area="Sta_Data_128", label="CampStat", dump_name="campstat", desc='''Campaign statistic
''')
        t.column('Campaign', 'character', format="x(8)", initial="", max_width=16, label="Campaign ID", column_label="ID", position=2, order=10, help="Campaign ID")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=3, order=20, help="Code Of Brand")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Customer", position=4, order=30, help="Customer's number")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, label="CLI", position=5, order=40, help="CLI")
        t.column('CampDate', 'date', format="99-99-99", max_width=4, label="Campaign Date", column_label="Date", position=6, order=50, help="Date when campaign was used")
        t.index('Campaign', [['Brand'], ['Campaign'], ['CustNum']], area="Sta_Index_2", primary=True)
        t.index('CampCLI', [['Brand'], ['Campaign'], ['CLI']], area="Sta_Index_2")
        t.index('CLI', [['Brand'], ['CLI'], ['CampDate', 'DESC']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['CampDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CampStat')
