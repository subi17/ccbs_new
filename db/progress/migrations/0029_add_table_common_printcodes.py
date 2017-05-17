from gearbox.migrations import Migration

class AddTablePrintCodes(Migration):

    database = "common"

    def up(self):
        t = self.table('PrintCodes', area="Sta_Data_32", label="Printer Control Codes", dump_name="printcod", desc="Printer control codes")
        t.column('Effect', 'character', format="x(1)", initial="", max_width=2, label="Code", column_label="Code", position=2, order=10, help="Code of a control character string")
        t.column('PrinterId', 'character', format="x(24)", initial="", max_width=48, label="Printer", column_label="Printer", position=3, order=15, help="Printer's logical name")
        t.column('EffOn', 'character', format="x(65)", initial="", max_width=264, label="Begin", column_label="Begin", extent=2, position=4, order=20, help="Print's start code")
        t.column('EffOff', 'character', format="x(65)", initial="", max_width=264, label="End", column_label="End", extent=2, position=5, order=30, help="Terminating control code sequence")
        t.column('EffName', 'character', format="x(30)", initial="", max_width=60, label="FX's name", column_label="FX's name", position=6, order=40, help="Descriptive name for a control code")
        t.column('PageWidth', 'integer', format="ZZ9", initial="0", max_width=4, label="PageWe", column_label="PageWe", position=7, order=50, help="Maximum no. of characters per line on printer")
        t.column('PageLength', 'integer', format="ZZ9", initial="0", max_width=4, label="PageLength", column_label="PageLength", position=8, order=60, help="Total lines per page")
        t.column('AvailLines', 'integer', format="ZZ9", initial="0", max_width=4, label="Rows", column_label="Rows", position=9, order=70, help="Number of lines available on page")
        t.index('Effect', [['Effect'], ['PrinterId']], area="Sta_Index_2", primary=True, unique=True)
        t.index('EffName', [['PrinterId'], ['EffName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PrintCodes')
