from gearbox.migrations import Migration

class AddTableprinter(Migration):

    database = "star"

    def up(self):
        t = self.table('printer', area="Sta_Data_256", dump_name="printer")
        t.column('PrinterCode', 'character', format="X(12)", initial="", max_width=24, label="Printer code", position=2, order=10, help="Visual name of printer")
        t.column('port', 'character', format="X(30)", initial="", max_width=60, label="Port", position=3, order=20, help="Fysical port name")
        t.column('command', 'character', format="X(50)", initial="", max_width=100, label="Command", position=4, order=30, help="Output through command")
        t.column('printerType', 'character', format="X(8)", initial="", max_width=16, label="Printer type", position=5, order=40, help="Printer type. Postscritp,PCL")
        t.index('main', [['PrinterCode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('printer')
