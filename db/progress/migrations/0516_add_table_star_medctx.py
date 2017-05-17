from gearbox.migrations import Migration

class AddTableMedCTX(Migration):

    database = "star"

    def up(self):
        t = self.table('MedCTX', area="Sta_Data_256", label="MedCTX", dump_name="medctx", desc="CDR preprosessor file CENTREX sections")
        t.column('CtxId', 'character', format="x(8)", initial="", max_width=16, label="Name", column_label="Name", position=2, order=10, help="Name of FreePhone Centrex Group")
        t.column('CtxFrom', 'character', format="x(12)", initial="", max_width=24, label="From", column_label="From", position=3, order=20, help="Number series FROM")
        t.column('CtxTo', 'character', format="x(12)", initial="", max_width=24, label="TO", column_label="TO", position=4, order=30, help="Number series TO")
        t.column('Ident', 'character', format="x(8)", initial="", max_width=16, label="Ident", column_label="Ident", position=5, order=80, help="Identification name of the switch / fidex")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=6, order=90, help="Explanation / memory field for centrex number series")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=7, order=100, help="Customer's number")
        t.index('Ident', [['Ident'], ['CtxId'], ['CtxFrom'], ['CtxTo']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CtxId', [['CtxId'], ['CtxFrom']], area="Sta_Index_2")
        t.index('CustNum', [['CustNum'], ['CtxId'], ['CtxFrom']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MedCTX')
