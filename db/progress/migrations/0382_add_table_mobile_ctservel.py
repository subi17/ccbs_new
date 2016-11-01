from gearbox.migrations import Migration

class AddTableCTServEl(Migration):

    database = "mobile"

    def up(self):
        t = self.table('CTServEl', area="Sta_Data_128_2", label="CLIType Service Elements", dump_name="ctservel", desc="Service elements of a CLI type")
        t.column('CLIType', 'character', format="x(8)", initial="", max_width=16, label="CLI Type", column_label="CLIType", position=2, order=20, help="CLI type")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=3, order=10, help="Code Of Brand")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Component", position=4, order=40, help="Code of Service Component")
        t.column('DefValue', 'integer', format=">>>9", initial="0", max_width=4, label="Default Value", column_label="Default", position=5, order=60, help="Default value")
        t.column('ChgAllowed', 'logical', format="Yes/No", initial="yes", max_width=1, label="Change Allowed", column_label="Changeable", position=6, order=70, help="Value can be changed on subscription level")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From Date", column_label="From", position=7, order=50, help="Valid from date")
        t.column('ServPac', 'character', format="x(12)", initial="", max_width=24, label="Service Package", column_label="ServPack", position=8, order=30, help="Service package code")
        t.column('CTServEl', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Element ID", column_label="ID", position=9, order=80, help="Unique ID")
        t.column('ServType', 'integer', format="9", initial="0", max_width=4, label="Service Type", column_label="ServType", position=10, order=90, help="Service type; basic / additional")
        t.column('DefParam', 'character', format="x(20)", initial="", max_width=40, label="Default Parameter", column_label="Parameter", position=11, order=100, help="Default value for parameter")
        t.index('CLIType', [['Brand'], ['CLIType'], ['ServPac'], ['ServCom'], ['FromDate', 'DESC']], area="Sta_Index_3", primary=True, unique=True)
        t.index('CTServEl', [['CTServEl']], area="Sta_Index_3", unique=True)
        t.index('ServCom', [['Brand'], ['ServCom'], ['CLIType'], ['FromDate', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('CTServEl')
