from gearbox.migrations import Migration

class AddTableSLItemParam(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SLItemParam', area="Sta_Data_64", dump_name="slitempa")
        t.column('SoLog', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Order Seq", column_label="Order Seq", position=2, order=10, help="Sequence for a Service Order")
        t.column('RequestId', 'integer', format=">>9", initial="0", max_width=4, label="RequestId", column_label="RId", position=3, order=20)
        t.column('ParamName', 'character', format="x(35)", initial="", max_width=16, label="ParamName", column_label="ParamName", position=4, order=30)
        t.column('ParamValue', 'character', format="x(35)", initial="", max_width=16, label="ParamValue", column_label="ParamValue", position=5, order=40)
        t.index('SoLog', [['SoLog'], ['RequestId']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('SLItemParam')
