from gearbox.migrations import Migration

class AddTableTCCError(Migration):

    database = "common"

    def up(self):
        t = self.table('TCCError', area="Sta_Data_128", label="TCC Error Configuration", dump_name="tccerror", desc="Technical CC Params")
        t.column('TCC', 'integer', mandatory=True, format="zzzz9", initial="0", max_width=5, label="TCC", column_label="TCC", position=2, order=10, help="Technical Call Case Number ")
        t.column('ErrorValue', 'character', format="x(30)", initial="", max_width=60, label="Value", column_label="Value", position=3, order=15, help="Value that triggers errorcode")
        t.column('ValidFrom', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="Valid From", position=16, order=60, help="The date from which this TCCError will be used.")
        t.column('ValidTo', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="Valid To", position=23, order=70, help="The date to (until) which this TCCError will be used.")
        t.column('TCCPName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=24, order=30, help="Name of TCC Parameter")
        t.column('ErrorCode', 'integer', mandatory=True, format="zzzz9", initial="0", max_width=5, label="TCC", column_label="Error Code", position=25, order=100, help="Error code")
        t.column('ValueType', 'character', format="x(30)", initial="", max_width=60, label="Value Type", column_label="Type", position=26, order=110, help="Type of the value that triggers errorcode")
        t.index('TCC', [['TCC'], ['ValueType'], ['ErrorValue'], ['ValidTo', 'DESC']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('TCCError')
