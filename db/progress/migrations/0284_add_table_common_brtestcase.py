from gearbox.migrations import Migration

class AddTableBRTestCase(Migration):

    database = "common"

    def up(self):
        t = self.table('BRTestCase', area="Sta_Data_128", label="BR Test Case", dump_name="brtestcase", desc="Billrun test case")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('BRTestCaseID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Case ID", column_label="Case ID", position=3, order=20, help="Test case ID")
        t.column('Description', 'character', format="x(50)", initial="", max_width=100, label="Description", position=4, order=30, help="Description of test case")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Result Billing Item", column_label="Bill.Item", position=5, order=40, help="Result billing item ID")
        t.column('ResultValue', 'decimal', format="->>>>>>>9.99", decimals=3, initial="0", max_width=18, label="Result Value", column_label="Result", position=6, order=50, help="Result value")
        t.column('RelationalOperator', 'character', format="x(8)", initial="", max_width=16, label="Relational Operator", column_label="Rel.Operator", position=7, order=60, help="Relational operator for the result")
        t.column('Active', 'logical', format="Yes/No", initial="no", max_width=1, label="Active", position=8, order=70, help="Is case active")
        t.column('ResultQty', 'integer', format="->>>>>>>9", initial="1", max_width=4, label="Result Quantity", column_label="Qty", position=9, order=80, help="Result quantity")
        t.index('BRTestCaseID', [['BRTestCaseID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('BrandID', [['Brand'], ['BRTestCaseID']], area="Sta_Index_1")
        t.index('Description', [['Brand'], ['Description']], area="Sta_Index_1")

    def down(self):
        self.drop_table('BRTestCase')
