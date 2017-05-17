from gearbox.migrations import Migration

class AddTableFuncRunProcess(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunProcess', area="Dyn_Data_64", label="Function process", dump_name="FuncRunProcess", desc="Single process of a function")
        t.column('FRConfigID', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="Conf.", position=2, order=20, help="Configuration ID")
        t.column('ProcSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Process Sequence", column_label="Sequence", position=3, order=30, help="Sequence of the process within the execution")
        t.column('ProcessID', 'integer', format=">>>>9", initial="0", max_width=4, label="OS Process ID", column_label="OS PID", position=4, order=40, help="OS process ID")
        t.column('Processed', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Processed Events", column_label="Events", position=7, order=70, help="Processed event qty")
        t.column('StartTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="Start Time", column_label="Started", position=8, order=80, help="Start time")
        t.column('EndTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="End Time", column_label="Ended", position=9, order=90, help="End time")
        t.column('LastTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="Last Update", column_label="Updated", position=10, order=100, help="Time of last update")
        t.column('ProcessHost', 'character', format="x(16)", initial="", max_width=32, label="Host Where Processed", column_label="Host", position=13, order=180, help="Host where this is processed")
        t.column('FRExecID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Execution ID", column_label="Exec.", position=14, order=140, help="ID of the execution")
        t.column('RunState', 'character', format="x(16)", initial="", max_width=32, label="Run Status", column_label="Status", position=15, order=150, help="Status of the process")
        t.column('FRProcessID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="FR Process ID", column_label="Proc.", position=16, order=160, help="Unique ID of the process")
        t.column('RunCommand', 'character', format="x(30)", initial="", max_width=40, label="Run Command", column_label="Command", position=17, order=170, help="Command for starting the session")
        t.index('FRConfigID', [['FRConfigID'], ['FRExecID'], ['ProcSeq']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('FRProcessID', [['FRProcessID']], area="Dyn_Index_1", unique=True)

    def down(self):
        self.drop_table('FuncRunProcess')
