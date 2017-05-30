from gearbox.migrations import Migration

class AddTableBRTestQRow(Migration):

    database = "common"

    def up(self):
        t = self.table('BRTestQRow', area="Sta_Data_2_256", label="BR Test Queue Row", dump_name="brtestqrow", desc="Billrun test queue row")
        t.column('BRTestQRowID', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Test Queue Row ID", column_label="Queue Row ID", position=2, order=10, help="Test queue row ID")
        t.column('BRTestQueueID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Queue ID", column_label="Queue ID", position=3, order=20, help="Billrun test queue ID")
        t.column('BRTestCaseID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Case ID", column_label="Case ID", position=4, order=30, help="Test case ID")
        t.column('CaseQty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Case Quantity", column_label="Case Qty", position=5, order=40, help="Quantity of cases to be picked")
        t.column('Active', 'logical', format="Yes/No", initial="no", max_width=1, label="Active", position=6, order=50, help="Is row active")
        t.index('BRTestQRowID', [['BRTestQRowID']], area="Sta_Index_4", primary=True, unique=True)
        t.index('BRTestCaseID', [['BRTestCaseID']], area="Sta_Index_4")
        t.index('BRTestQueueID', [['BRTestQueueID'], ['BRTestCaseID']], area="Sta_Index_4")

    def down(self):
        self.drop_table('BRTestQRow')
