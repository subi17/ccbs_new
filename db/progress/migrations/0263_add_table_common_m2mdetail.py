from gearbox.migrations import Migration

class AddTableM2MDetail(Migration):

    database = "common"

    def up(self):
        t = self.table('M2MDetail', area="Sta_Data_128", dump_name="m2mdetai")
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="CLI", column_label="CLI", position=2, order=40, help="CLI, subscriber no.")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=3, order=20, help="Sequence for a Subscription")
        t.column('RejCode', 'character', format="x(2)", initial="", max_width=4, label="RejCode", column_label="RejCode", position=4, order=200, help="Rejection code")
        t.column('RejText', 'character', format="x(20)", initial="", max_width=40, label="RejText", column_label="RejText", position=5, order=210, help="Rejection text")
        t.column('ReqStatus', 'integer', format=">>9", initial="999", max_width=4, label="Status", column_label="Status", position=6, order=140)
        t.column('M2MRequest', 'integer', format=">>>>>>9", initial="0", max_width=4, position=7, order=220)
        t.index('M2MRequest', [['M2MRequest']], area="Sta_Index_2", primary=True)
        t.index('MsSeq', [['MsSeq']], area="Sta_Index_2")

    def down(self):
        self.drop_table('M2MDetail')
