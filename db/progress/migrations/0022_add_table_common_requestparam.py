from gearbox.migrations import Migration

class AddTableRequestParam(Migration):

    database = "common"

    def up(self):
        t = self.table('RequestParam', area="Sta_Data_64", label="Request Parameters", dump_name="requestparam", desc="Request parameters")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('ReqType', 'integer', format=">>9", initial="0", max_width=4, label="Request Type", column_label="Type", position=3, order=20, description="Request type")
        t.column('ParamField', 'character', format="x(12)", initial="", max_width=24, label="Parameter", position=4, order=30, help="Parameter field")
        t.column('Usage', 'character', format="x(30)", initial="", help="Usage, i.e. contents of parameter", max_width=60, label="Usage", position=5, order=40, description='''

''')
        t.column('IntConfig', 'integer', format=">>>>9", initial="0", max_width=4, label="Configuration", position=6, order=50, help="Configuration")
        t.column('CharConfig', 'character', format="x(30)", initial="", max_width=60, label="Configuration", position=7, order=60, help="Configuration")
        t.column('DispParam', 'logical', format="Yes/No", initial="yes", max_width=1, label="Display Parameter", column_label="Display", position=8, order=70, help="Display parameter in UI")
        t.column('Description', 'character', format="x(50)", initial="", max_width=100, label="Description", position=9, order=80, help="Description")
        t.index('ParamField', [['Brand'], ['ReqType'], ['ParamField']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('RequestParam')
