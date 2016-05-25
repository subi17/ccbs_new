from gearbox.migrations import Migration

class AddTableSLGAnalyse(Migration):

    database = "common"

    def up(self):
        t = self.table('SLGAnalyse', area="Sta_Data_64", dump_name="slganaly")
        t.column('Clitype', 'character', format="x(8)", initial="", max_width=16, label="CLI Type", column_label="CLIType", position=2, order=20, help="CLI Type")
        t.column('ServiceLimitGroup', 'character', format="x(16)", initial="", max_width=32, label="ServiceLimit Group", column_label="ServiceLimitGroup", position=3, order=70, help="ServiceLimitGroup")
        t.column('CCN', 'integer', format=">>9", initial="0", max_width=4, label="CCN", column_label="CCN", position=4, order=40, help="CCN")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=12, order=170, help="Code Of Brand")
        t.column('ValidFrom', 'date', format="99-99-9999", max_width=4, label="Valid From", column_label="Valid From", position=16, order=50, help="The date FROM which this SLG analyse will be used.")
        t.column('BelongTo', 'logical', format="+/-", initial="TRUE", max_width=1, label="BelongTo", column_label="BelongTo", position=17, order=10, help="Belong to ServicelimitGroup")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillCode", position=18, order=30, help="Billing item code, max 16 characters")
        t.column('BDest', 'character', format="x(16)", initial="", max_width=32, label="B-subNo", column_label="B-subNo", position=19, order=45, help="B-subscriber/destination ")
        t.column('ValidTo', 'date', format="99-99-9999", max_width=4, label="Valid To", column_label="Valid To", position=20, order=60, help="The date TO which this SLG analyse will be used.")
        t.column('SLGAType', 'integer', format="9", initial="0", max_width=4, label="Type", column_label="Type", position=21, order=160, help="Service type")
        t.column('Prior', 'integer', format=">>9", initial="0", max_width=4, label="Prior", column_label="Priority", position=22, order=120, help="Priority")
        t.index('BillCode', [['Brand'], ['BillCode'], ['Clitype'], ['ValidTo', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('BelongTo', [['Brand'], ['BelongTo'], ['Clitype'], ['BillCode'], ['CCN'], ['BDest'], ['Prior'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('CliType', [['Brand'], ['Clitype'], ['BillCode'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('ServiceLimitGroup', [['Brand'], ['ServiceLimitGroup'], ['Clitype'], ['ValidTo', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('SLGAnalyse')
