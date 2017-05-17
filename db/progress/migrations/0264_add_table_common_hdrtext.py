from gearbox.migrations import Migration

class AddTableHdrText(Migration):

    database = "common"

    def up(self):
        t = self.table('HdrText', area="Sta_Data_128", label="HdrText", dump_name="hdrtext", desc="Header texts for reports")
        t.column('te-nro', 'integer', format="zz9", initial="0", max_width=4, label="No", column_label="No.", position=2, order=10, help="Serial number(1 - 999) of a text element")
        t.column('te-kie', 'integer', mandatory=True, format="9", initial="0", max_width=4, label="Lang", column_label="Lang", position=3, order=20, help="Language code (1 - 9)")
        t.column('te-text', 'character', format="x(60)", initial="", max_width=120, label="Text", column_label="Text", position=4, order=30, help="Text in choosen language")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('te-nro', [['Brand'], ['te-nro'], ['te-kie']], area="Sta_Index_2", primary=True, unique=True)
        t.index('te-kie', [['Brand'], ['te-kie'], ['te-nro']], area="Sta_Index_2", unique=True)
        t.index('te-text', [['Brand'], ['te-text']], area="Sta_Index_2")

    def down(self):
        self.drop_table('HdrText')
