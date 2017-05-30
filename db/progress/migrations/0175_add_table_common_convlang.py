from gearbox.migrations import Migration

class AddTableConvLang(Migration):

    database = "common"

    def up(self):
        t = self.table('ConvLang', area="Sta_Data_256", label="Conversion Language", dump_name="convlang", desc="Conversion languages")
        t.column('CcLang', 'character', format="x(8)", initial="", max_width=16, label="Conv. language", column_label="Conv.", position=2, order=10, help="Conversion language")
        t.column('CcLName', 'character', format="x(30)", initial="", max_width=60, label="Name", position=3, order=20, help="Conversion language name")
        t.index('CcLang', [['CcLang']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CcLName', [['CcLName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ConvLang')
