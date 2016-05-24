from gearbox.migrations import Migration

class AddTableTMSPrinter(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSPrinter', area="Sta_Data_64", label="Printers", dump_name="tmsprint", desc="Printers")
        t.column('PrinterId', 'character', format="x(24)", initial="", max_width=48, label="Mnemonic", column_label="Mnemonic", position=2, order=10, help="Printer's logical name")
        t.column('Device', 'character', format="x(30)", initial="", max_width=60, label="FysName", column_label="FysName", position=3, order=20, help="Printer's device name")
        t.column('PageLength', 'integer', format="ZZ9", initial="0", max_width=4, label="PageSize", column_label="PageSize", position=4, order=30, help="Total length of page (default)")
        t.column('PageAvail', 'integer', format="ZZ9", initial="0", max_width=4, label="Rows", column_label="Rows", position=5, order=40, help="Lines available on one page(default)")
        t.column('PageWidth', 'integer', format="ZZ9", initial="0", max_width=4, label="PageWidth", column_label="PageWidht", position=6, order=50, help="Default line width on printer ")
        t.column('LogCode', 'character', format="X(100)", initial="", max_width=1010, label="Logical code", extent=5, position=7, order=60, help="Logical name for printer")
        t.column('DeviceCode', 'character', format="X(100)", initial="", max_width=1010, label="Physical code", extent=5, position=8, order=70, help="Physical device")
        t.column('UseScript', 'logical', format="yes/no", initial="no", help="Use Unix scripts in output command", max_width=1, label="Use scripts", column_label="Scripts", position=9, order=80, description="If true  then ""output through"" is used ")
        t.index('PrinterId', [['PrinterId', 'ABBREVIATED']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TMSPrinter')
