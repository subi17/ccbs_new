from gearbox.migrations import Migration

class AddTableCLIWL(Migration):

    database = "star"

    def up(self):
        t = self.table('CLIWL', area="Schema Area", label="CLIWL", dump_name="cliwl", desc="Used to store the filename of the list that the switch will read.")
        t.column('FileName', 'character', format="X(14)", initial="", help="File name", max_width=28, label="Filename", column_label="Filename", position=2, order=10, description="Name of the generated file that the switch will read")
        t.column('OrderId', 'character', format="X(8)", initial="", help="Order id", max_width=16, label="OrderId", column_label="OrderId", position=3, order=20, description="The OrderId of the requestfile")
        t.column('Processed', 'logical', format="yes/no", initial="no", help="Processed", max_width=1, label="Processed", column_label="Processed", position=4, order=30, description="Indicates whether the logfile from the switch, has been processed, or not.")
        t.column('ExCode', 'character', format="x(6)", initial="", max_width=12, label="Switch", column_label="Switch", position=5, order=100, help="Name of the switch this queue belongs to")
        t.index('filename', [['FileName'], ['ExCode'], ['Processed']], area="Schema Area", primary=True)

    def down(self):
        self.drop_table('CLIWL')
