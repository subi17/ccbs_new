from gearbox.migrations import Migration

class AddTableFuncRunParam(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunParam', area="Sta_Data_128", label="Function Config Param", dump_name="FuncRunParam", desc="Parameters for function configuration")
        t.column('FRConfigID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="Conf.", position=2, order=10, help="Unique ID for configuration")
        t.column('ParamType', 'character', format="x(8)", initial="", max_width=16, label="Parameter Type", column_label="Type", position=3, order=20, help="Parameter type")
        t.column('ParamName', 'character', format="x(20)", initial="", max_width=40, label="Parameter Name", column_label="Name", position=4, order=30, help="Parameter name")
        t.column('ParamSeq', 'integer', format=">>9", initial="0", max_width=4, label="Parameter Sequence", column_label="Sequence", position=5, order=40, help="Parameter sequence (order)")
        t.column('DefaultValue', 'character', format="x(20)", initial="", max_width=40, label="Default Value", column_label="Default", position=6, order=50, help="Default value for parameter")
        t.index('ParamSeq', [['FRConfigID'], ['ParamSeq']], area="Sta_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('FuncRunParam')
