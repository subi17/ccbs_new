from gearbox.migrations import Migration

class AddTableSLItem(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SLItem', area="Sta_Data_64", dump_name="slitem")
        t.column('SoLog', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Order Seq", column_label="Order Seq", position=2, order=10, help="Sequence for a Service Order")
        t.column('RequestId', 'integer', format=">>9", initial="0", max_width=4, label="Id.", column_label="Id.", position=3, order=20)
        t.column('ParamName', 'character', format="x(32)", initial="", max_width=16, label="ParamName", column_label="ParamName", position=4, order=30)
        t.column('ItemStatus', 'integer', format=">>9", initial="0", max_width=4, label="ItemStatus", column_label="St.", position=5, order=40)
        t.column('ErrorMessage', 'character', format="x(31)", initial="", max_width=60, label="ErrorMessage", column_label="ErrorMessage", position=7, order=60)
        t.column('ErrorCode', 'integer', format=">>9", initial="0", max_width=4, label="ErrorCode", column_label="E.C", position=8, order=70)
        t.column('TMSParam', 'character', format="x(8)", initial="", max_width=16, label="TMSParam", column_label="TMSParam", position=9, order=80)
        t.index('SoLog', [['SoLog']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('SLItem')
