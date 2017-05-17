from gearbox.migrations import Migration

class AddTableFuncRunResult(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunResult', area="Dyn_Data_128", label="Function feed", dump_name="FuncRunResult", desc="Feed to a function process")
        t.column('CharParam', 'character', format="x(8)", initial="", max_width=16, label="Character Parameter", column_label="Char.Param", position=3, order=20, help="Character parameter")
        t.column('IntParam', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Integer Parameter", column_label="Int.Param", position=4, order=30, help="Integer parameter")
        t.column('FRProcessID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="FR Process ID", column_label="Proc.", position=5, order=40, help="Unique ID of the process")
        t.column('ResultOrder', 'integer', format=">>>>>>>>>>9", initial="0", max_width=4, label="Result Order", column_label="Order", position=6, order=60, help="Order of the result rows")
        t.column('DecParam', 'decimal', format="->>>>>>>>9.99999", decimals=5, initial="0", max_width=20, label="Decimal Parameter", column_label="Dec.Param", position=7, order=50, help="Decimal parameter")
        t.column('FRResultSeq', 'integer', format=">>>>>9", initial="0", max_width=4, label="Sequence", column_label="Seq.", position=8, order=80, help="Sequence (order) within one execution")
        t.column('FRExecID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Execution ID", column_label="Exec.", position=9, order=70, help="Execution from which the feeds were created")
        t.column('ProcessHost', 'character', format="x(16)", initial="", max_width=32, label="Host Where Processed", column_label="Host", position=10, order=90, help="Host where this result is processed")
        t.index('FRProcessID', [['FRProcessID'], ['FRResultSeq'], ['ResultOrder']], area="Dyn_Index_1", primary=True)
        t.index('FRExecID', [['FRExecID'], ['FRResultSeq']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('FuncRunResult')
