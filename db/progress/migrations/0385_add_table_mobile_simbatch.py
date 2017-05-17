from gearbox.migrations import Migration

class AddTableSimBatch(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SimBatch', area="Sta_Data_128_2", dump_name="simbatch", desc='''
''')
        t.column('SimBatch', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Batch#", column_label="Batch#", position=2, order=10, help="Sequence for a Simbatch")
        t.column('ManCode', 'character', format="x(8)", initial="", max_width=16, label="Manufacturer", column_label="Manufacturer", position=3, order=20, help="Code of Manufacturer")
        t.column('SimArt', 'character', format="x(12)", initial="", max_width=24, label="Sim Article", column_label="Sim Article", position=4, order=30, help="Article Code for a SIM type")
        t.column('TpKey', 'character', format="x(8)", initial="", max_width=16, label="TransKey", column_label="TransKey", position=5, order=40, help="Transport key")
        t.column('DelDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Delivery Date", column_label="Delivery Date", position=6, order=50, help="Day of Delivery")
        t.column('memo', 'character', format="x(60)", initial="", max_width=1220, label="Memo", column_label="Memo", extent=10, position=7, order=60, help="Memo of ServPack")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=8, order=70, help="Code Of Brand")
        t.column('FileName', 'character', format="X(20)", initial=self.unknown, max_width=80, label="FileName", column_label="FileName", position=9, order=80, help="Name of the read sim file")
        t.index('ManCode', [['Brand'], ['ManCode'], ['DelDate']], area="Sta_Index_3", primary=True)
        t.index('FileName', [['Brand'], ['ManCode'], ['FileName']], area="Sta_Index_3")
        t.index('SimArt', [['Brand'], ['SimArt'], ['DelDate']], area="Sta_Index_3")
        t.index('SimBatch', [['Brand'], ['SimBatch']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('SimBatch')
