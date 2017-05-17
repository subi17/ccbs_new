from gearbox.migrations import Migration

class AddTableBRTestQResult(Migration):

    database = "common"

    def up(self):
        t = self.table('BRTestQResult', area="Sta_Data_2_256", label="BR Test Queue Result", dump_name="brtestqresult", desc="Billrun test queue result")
        t.column('BRTestQueueID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Queue ID", column_label="Queue ID", position=2, order=10, help="Billrun test queue ID")
        t.column('BRTestQResultID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Queue Result ID", column_label="Result ID", position=3, order=20, help="Test queue result ID")
        t.column('TestRunStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Test Run Time", column_label="Time", position=4, order=30, help="Test run time")
        t.column('UserCode', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="User ID", position=5, order=40, help="User ID")
        t.index('BRTestQResultID', [['BRTestQResultID']], area="Sta_Index_4", primary=True, unique=True)
        t.index('BRTestQueueID', [['BRTestQueueID'], ['TestRunStamp', 'DESC']], area="Sta_Index_4")

    def down(self):
        self.drop_table('BRTestQResult')
