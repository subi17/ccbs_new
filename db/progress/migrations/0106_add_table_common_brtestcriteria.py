from gearbox.migrations import Migration

class AddTableBRTestCriteria(Migration):

    database = "common"

    def up(self):
        t = self.table('BRTestCriteria', area="Sta_Data_64", label="BR Test Criteria", dump_name="brtestcriteria", desc="Billrun test case criteria")
        t.column('BRTestCaseID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Case ID", column_label="Case ID", position=2, order=10, help="Test case ID")
        t.column('BRTestCriteriaID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Criteria ID", column_label="Criteria ID", position=3, order=20, help="Test criteria id")
        t.column('CriteriaTable', 'character', format="x(20)", initial="", max_width=40, label="Criteria Table", column_label="Table", position=4, order=30, help="Criteria table")
        t.column('CriteriaField', 'character', format="x(20)", initial="", max_width=40, label="Criteria Field", column_label="Field", position=5, order=40, help="Criteria field")
        t.column('ValueIncluded', 'character', format="x(20)", initial="", max_width=40, label="Value Included", column_label="Included", position=6, order=50, help="Value included")
        t.column('ROValueIncluded', 'character', format="x(8)", initial="", max_width=16, label="Rel. Operator For Value", column_label="Value Operator", position=8, order=70, help="Relational operator for value included")
        t.column('Setting', 'character', format="x(20)", initial="", max_width=40, label="Setting", position=9, order=80, help="Setting")
        t.column('Active', 'logical', format="Yes/No", initial="yes", max_width=1, label="Active", position=12, order=110, help="Is case active")
        t.column('EventDateFrom', 'character', format="x(20)", initial="", max_width=40, label="Event Date From", column_label="Date From", position=14, order=130, help="Event date from")
        t.column('EventDateTo', 'character', format="x(20)", initial="", max_width=40, label="Event Date To", column_label="Date To", position=15, order=140, help="Event date to")
        t.column('BRTestQueueID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Queue ID", column_label="Queue ID", position=16, order=150, help="Billrun test queue ID")
        t.column('CriteriaOwner', 'character', format="x(12)", initial="", max_width=24, label="Criteria Owner", column_label="Owner", position=17, order=160, help="Criteria owner")
        t.index('BRTestCriteriaID', [['BRTestCriteriaID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('BRTestCaseID', [['BRTestCaseID'], ['Active'], ['CriteriaTable'], ['CriteriaField']], area="Sta_Index_1")
        t.index('BRTestQueueID', [['BRTestQueueID'], ['Active'], ['CriteriaTable'], ['CriteriaField']], area="Sta_Index_1")

    def down(self):
        self.drop_table('BRTestCriteria')
