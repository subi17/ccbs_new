from gearbox.migrations import Migration

class AddTableMNPRetPlatform(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPRetPlatform', area="Sta_Data_128", label="MNPRetPlatform", dump_name="mnpretplatform", desc="MNP retention platform configuration")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of Brand")
        t.column('RetentionPlatform', 'character', format="x(12)", initial="0", max_width=24, label="RetentionPlatform", position=3, order=20)
        t.column('Percentage', 'decimal', format="->>>9.99", decimals=5, initial="0", max_width=20, label="Percentage", position=4, order=30, help="Percentage of the load of the platform")
        t.column('SMSSender', 'character', format="x(11)", initial="", max_width=22, label="SMSSender", position=5, order=40, help="SMS sender number")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="FromDate", position=6, order=50, help="Date when validity expires")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="ToDate", position=7, order=60, help="Date when validity begins")
        t.column('Name', 'character', format="x(15)", initial="", max_width=30, label="Name", position=8, order=70, help="Platform name")
        t.index('MNPRetPlatform', [['RetentionPlatform']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ToDate', [['Brand'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPRetPlatform')
