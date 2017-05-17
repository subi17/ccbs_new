from gearbox.migrations import Migration

class AddTableMNPOperator(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPOperator', area="Sta_Data_128", dump_name="mnpoperator")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('OperName', 'character', format="X(20)", initial="", max_width=40, label="Operator Name", column_label="OperName", position=3, order=20)
        t.column('OperCode', 'character', format="X(8)", initial="", max_width=16, label="Operator Code", column_label="OperCode", position=4, order=30, help="Operator code (RRC)")
        t.column('ICCPrefix', 'character', format="X(8)", initial="", max_width=16, label="ICC Prefix", column_label="ICCPrefix", position=5, order=40)
        t.column('Active', 'logical', format="Yes/No", initial="no", max_width=1, label="Active", column_label="Active", position=6, order=50, help="Is operator active or not (available in order channels)")
        t.column('CancelAgreement', 'logical', format="Yes/No", initial="no", max_width=1, label="Cancellation Agreement", column_label="CancelAgr", position=7, order=60, help="Affects how late MNP cancellation can be done")
        t.column('OperBrand', 'character', format="X(20)", initial="", max_width=40, label="Operator Brand", column_label="OperBrand", position=8, order=70, help="Common brand of for operators with the same operator code")
        t.column('NRN', 'character', format="x(8)", initial="", max_width=16, label="NRN", column_label="NRN", position=9, order=80, help="NRN code")
        t.index('OperCode', [['Brand'], ['OperCode'], ['OperName']], area="Sta_Index_1", primary=True, unique=True)
        t.index('OperName', [['Brand'], ['OperName']], area="Sta_Index_1")

    def down(self):
        self.drop_table('MNPOperator')
