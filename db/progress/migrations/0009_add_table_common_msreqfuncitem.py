from gearbox.migrations import Migration

class AddTableMsReqFuncItem(Migration):

    database = "common"

    def up(self):
        t = self.table('MsReqFuncItem', area="Sta_Data_2_256", dump_name="MsReqFuncItem")
        t.column('ItemId', 'character', format="x(5)", initial="0", max_width=10, label="FuncItemId", column_label="StatFuncItemId", position=2, order=10, description="ReqStat function item identifier")
        t.column('ItemDesc', 'character', format="x(30)", initial="0", max_width=60, label="ItemDesc", column_label="ItemDesc", position=3, order=20, description="Description of items function")
        t.column('Module', 'character', format="x(12)", initial="", max_width=24, label="Module", column_label="Module", position=4, order=30, description="Code / Action")
        t.column('IParam', 'integer', format="zz9", initial="0", max_width=4, label="IParam", column_label="IParam", position=5, order=40, description="Parameter(integer)")
        t.column('CParam', 'character', format="CHR(20)", initial="", max_width=14, label="CParam", column_label="CParam", position=6, order=50, description="Parameter (character)")
        t.index('ItemId', [['ItemId']], area="Sta_Index_3", primary=True)

    def down(self):
        self.drop_table('MsReqFuncItem')
