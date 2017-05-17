from gearbox.migrations import Migration

class AddTableMXItem(Migration):

    database = "common"

    def up(self):
        t = self.table('MXItem', area="Sta_Data_128", dump_name="MXItem")
        t.column('MXName', 'character', format="x(16)", initial="", max_width=60, label="Name MXItem", column_label="Matrix item", position=3, order=20)
        t.column('MXSeq', 'integer', format=">>>>>>>>9", initial="0", help="Matrix Sequence", max_width=32, label="MXSeq", column_label="MXSeq", position=4, order=10, description="Matrix Key")
        t.column('MXValue', 'character', format="X(18)", initial="", help="Matrix Value", max_width=32, label="MXValue", column_label="MXValue", position=5, order=30, description="Matrix Value")
        t.index('MXSeq', [['MXSeq'], ['MXName']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('MXItem')
