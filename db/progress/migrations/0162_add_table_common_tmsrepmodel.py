from gearbox.migrations import Migration

class AddTableTMSRepModel(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSRepModel', area="Sta_Data_256", label="Report Models", dump_name="tmsrepmo", desc="Report models")
        t.column('RepNum', 'integer', format=">>9", initial="0", max_width=4, label="Report nbr", position=2, order=10, help="Report number")
        t.column('RepName', 'character', format="X(30)", initial="", max_width=60, label="Report Name", column_label="Name", position=3, order=20, help="Name of Report")
        t.column('PrinterId', 'character', format="x(24)", initial="", max_width=48, label="Logical Name", column_label="Logical", position=4, order=30, help="Logical name of the printer")
        t.column('Effect', 'character', format="x(1)", initial="", max_width=2, label="Effect", position=5, order=40, help="Effect")
        t.column('PageLength', 'integer', format=">>9", initial="0", max_width=4, label="Lines per page", column_label="Lines", position=6, order=50, help="Lines per one page (max)")
        t.column('AvailLines', 'integer', format=">>9", initial="0", max_width=4, label="Lines available", column_label="Available", position=7, order=60, help="Lines available on one page")
        t.column('UpdPerm', 'logical', format="Yes/No", initial="yes", max_width=1, label="Show print options", column_label="Show options", position=8, order=70, help="Show the printing options for user when starting to print")
        t.index('RepNum', [['RepNum']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TMSRepModel')
