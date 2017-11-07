from gearbox.migrations import Migration

class AddTableActionFlag(Migration):

    database = "common"

    def up(self):
        t = self.table('ActionFlag', area="Sta_Data_256", dump_name="actionfl")
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Subscription", column_label="MsSeq", position=2, order=10, help="Link to subscription")
        t.column('ActionType', 'integer', format=">9", initial="0", max_width=4, label="Action Type", column_label="Type", position=3, order=20, help="Action type")
        t.column('Brand', 'character', format="X(8)", initial="", max_width=16, label="Brand", position=4, order=30, help="Brand")
        t.column('TimeStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Time Stamp", column_label="Time", position=5, order=40, help="Time stamp")
        t.column('Memo', 'character', format="X(300)", initial="", max_width=600, label="Memo", position=6, order=50, help="Memo")
        t.index('MsSeq', [['MsSeq'], ['ActionType'], ['TimeStamp', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('ActionType', [['Brand'], ['ActionType']], area="Sta_Index_2")
        t.index('Brand', [['Brand'], ['TimeStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ActionFlag')
