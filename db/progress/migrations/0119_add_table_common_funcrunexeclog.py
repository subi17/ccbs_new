from gearbox.migrations import Migration

class AddTableFuncRunExecLog(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunExecLog', area="Dyn_Data_128", label="Function status", dump_name="FuncRunExecLog", desc="Status of a function")
        t.column('FRExecID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Execution ID", column_label="Exec.", position=2, order=10, help="Unique ID for the execution")
        t.column('StatusStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Time Stamp", column_label="Time", position=3, order=20, help="Time of the status change")
        t.column('FRStatus', 'character', format="x(20)", initial="", max_width=40, label="Status", position=4, order=30, help="Status")
        t.index('FRExecID', [['FRExecID'], ['StatusStamp', 'DESC']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('FuncRunExecLog')
