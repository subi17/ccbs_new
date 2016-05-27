from gearbox.migrations import Migration

class AddTableFuncRunQSParam(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunQSParam', area="Sta_Data_64", label="Function Queue Parameters", dump_name="FuncRunQSParam", desc="Parameters for scheduled function queue")
        t.column('FRQueueID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Queue ID", column_label="Queue", position=2, order=10, help="Unique ID for the queue")
        t.column('FRQRowSeq', 'integer', format=">>>>>9", initial="0", max_width=4, label="Row Sequence", column_label="Row", position=3, order=20, help="Row sequence (order)")
        t.column('FRQScheduleID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Queue Timing ID", column_label="Timing", position=4, order=30, help="Unique ID for scheduling row")
        t.column('CharParam', 'character', format="x(20)", initial="", max_width=40, label="Character Parameter", column_label="Char", position=5, order=50, help="Default value for character parameter")
        t.column('DateParam', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Date Parameter", column_label="Date", position=6, order=60, help="Default value for date parameter")
        t.column('DecParam', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Decimal Parameter", column_label="Dec", position=7, order=70, help="Default value for decimal parameter")
        t.column('IntParam', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Parameter", column_label="Int", position=8, order=80, help="Default value for integer parameter")
        t.column('LogParam', 'logical', format="Yes/No", initial="no", max_width=1, label="Logical Parameter", column_label="Logic", position=9, order=90, help="Default value for logical parameter")
        t.column('FRConfigID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="Conf.", position=10, order=100, help="Unique ID for configuration")
        t.column('ParamSeq', 'integer', format=">>9", initial="0", max_width=4, label="Parameter Sequence", column_label="Sequence", position=11, order=40, help="Parameter sequence (order)")
        t.column('ParamType', 'character', format="x(8)", initial="", max_width=16, label="Parameter Type", column_label="Type", position=12, order=120, help="Parameter type")
        t.index('FuncRunQSchedule', [['FRQScheduleID'], ['FRQRowSeq'], ['ParamSeq']], area="Sta_Index_1", primary=True, unique=True)
        t.index('FuncRunQRow', [['FRQueueID'], ['FRQRowSeq'], ['ParamSeq']], area="Sta_Index_1")

    def down(self):
        self.drop_table('FuncRunQSParam')
