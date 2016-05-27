from gearbox.migrations import Migration

class AddTableMatrix(Migration):

    database = "common"

    def up(self):
        t = self.table('Matrix', area="Sta_Data_128", dump_name="Matrix")
        t.column('Brand', 'character', format="X(8)", initial="1", max_width=4, label="Brand", column_label="Brand", position=2, order=10, description="Brand")
        t.column('MXName', 'character', format="x(30)", initial="", max_width=60, label="Name of the Matrix", column_label="Matrix Name", position=3, order=20)
        t.column('Prior', 'integer', format=">>9", initial="0", help="Matrix Prior", max_width=32, label="Prior", column_label="Prior", position=4, order=60, description="Matrix Prior")
        t.column('MXKey', 'character', format="x(8)", initial="", help="Matrix key", max_width=32, label="MXKey", column_label="MXKey", position=5, order=30, description="Matrix Key")
        t.column('MXSeq', 'integer', format=">>>>>>>>9", initial="0", help="Matrix Sequence", max_width=32, label="MXSeq", column_label="MXSeq", position=6, order=40, description="Matrix Key")
        t.column('MXRes', 'integer', format=">9", initial=self.unknown, help="Matrix Response", max_width=32, label="MXRes", column_label="MXRes", position=7, order=50, description="Matrix Response")
        t.index('Brand', [['Brand'], ['MXKey'], ['Prior']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('Matrix')
