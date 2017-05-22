from gearbox.migrations import Migration

class AddTableOperIndir(Migration):

    database = "common"

    def up(self):
        t = self.table('OperIndir', area="Sta_Data_256", label="Operator's prefixes", dump_name="operindi", desc="Other operators indirect prefixes")
        t.column('Operator', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="Operator", column_label="Operator", position=2, order=10, help="Operator's code, 1 - 8 characters")
        t.column('Prefix', 'character', format="x(5)", initial="", max_width=10, label="Indirect Prefix", column_label="Indirect Prefix", position=3, order=60, help="Operators indirect prefix")
        t.column('Billable', 'logical', format="Y/N", initial="no", max_width=1, label="B", column_label="B", position=4, order=70, help="Will the indirect calls be billed from this operator ?")
        t.column('DestType', 'integer', format=">9", initial="0", max_width=4, label="Type", column_label="Type", position=5, order=130, help="B-subscriber type, used with FTAM server.")
        t.column('Absolute', 'logical', format="Yes/No", initial="no", max_width=1, label="Abs.", column_label="Abs.", position=6, order=140, help="Is the b-number type analyse absolute ?")
        t.index('Operator', [['Operator'], ['Prefix']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('OperIndir')
