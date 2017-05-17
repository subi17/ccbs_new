from gearbox.migrations import Migration

class AddFieldDPMember(Migration):

    database = "common"

    def up(self):
        self.sequence('DPMemberID', initial=0, min_val=0, cycle_on_limit="no", increment=1)
        t = self.alter_table('DPMember')
        t.column('DPMemberId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="DPMemberId", column_label="DPMemberId", position=9, order=80, help="Discount plan member Id")
        t.index('DPMemberId', [['DPMemberId']], area="Sta_Index_4", unique=True)

    def down(self):
        self.drop_sequence('DPMemberID')
        t = self.alter_table('DPMember')
        t.drop_index('DPMemberId')
        t.drop_column('DPMemberId')
