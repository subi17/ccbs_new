from gearbox.migrations import Migration

class AddTableTMSRepCfg(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSRepCfg', area="Sta_Data_128", label="Report Configuration", dump_name="tmsrepcf", desc="Report configuration")
        t.column('RepName', 'character', format="x(8)", initial="", max_width=16, label="Print.name", column_label="Print.name", position=2, order=10, help="Name of a printout")
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="User ID", column_label="User ID", position=3, order=20, help="User ID, 1 - 8 characters")
        t.column('PrinterId', 'character', format="x(24)", initial="", max_width=48, label="Printer", column_label="Printer", position=4, order=30, help="Logical name for printer")
        t.column('Effect', 'character', format="x(1)", initial="", max_width=2, label="FX", column_label="FX", position=5, order=40, help="Code of a control code sequence")
        t.column('UpdPerm', 'logical', format="Yes/No", initial="yes", max_width=1, label="Ask", column_label="Ask", position=6, order=90, help="Ask printer's setup when starting to print (Y/N)")
        t.column('RepNum', 'integer', format=">>9", initial="0", max_width=4, label="Report nbr", position=7, order=100, help="Report number")
        t.index('RepName', [['RepName'], ['UserCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('PrinterId', [['PrinterId']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TMSRepCfg')
