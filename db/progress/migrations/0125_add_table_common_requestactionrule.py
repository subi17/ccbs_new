from gearbox.migrations import Migration

class AddTableRequestActionRule(Migration):

    database = "common"

    def up(self):
        t = self.table('RequestActionRule', area="Sta_Data_64", label="Request Action Rule", dump_name="requestactionrule", desc="Request action rule")
        t.column('RequestActionID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Request Action ID", column_label="ID", position=2, order=10, help="Request action ID")
        t.column('ParamField', 'character', format="x(12)", initial="", max_width=24, label="Parameter", position=3, order=20, help="Parameter field")
        t.column('ParamValue', 'character', format="x(20)", initial="", max_width=40, label="Parameter Value", column_label="Value", position=4, order=30, help="Parameter value")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Date when rule becomes effective")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To", position=6, order=50, help="Date when usage of this rule ends")
        t.column('ExclParamValue', 'character', format="x(30)", initial="", max_width=60, label="Excluded Parameter Value", column_label="Excl.Value", position=7, order=60, help="Excluded parameter value")
        t.index('ParamField', [['RequestActionID'], ['ParamField'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('RequestActionRule')
