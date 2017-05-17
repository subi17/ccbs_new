from gearbox.migrations import Migration

class AddTableServiceLimitTarget(Migration):

    database = "common"

    def up(self):
        t = self.table('ServiceLimitTarget', area="Sta_Data_128", dump_name="servlt")
        t.column('SLSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="SLSeq", column_label="SLseq", position=2, order=10, help="Sequence for Servicelimit")
        t.column('ServiceLMember', 'character', format="x(16)", initial="", max_width=32, label="ServiceLimit Member", column_label="ServiceLimit Member", position=3, order=20, help="Momber of Service Limit")
        t.column('ServiceLimitMT', 'integer', format="9", initial="0", max_width=4, label="Type", column_label="Type", position=4, order=30, help="Member Type of Service Limit")
        t.column('OutsideRate', 'character', format="x(8)", initial="", max_width=16, label="OutsideRate", column_label="OutsideRate", position=5, order=40, help="Rate-key when limit is full")
        t.column('InsideRate', 'character', format="x(8)", initial="", max_width=16, label="LimitRate", column_label="LimitRate", position=6, order=50, help="Rate-key when belongs to group")
        t.index('SLSeq', [['SLSeq'], ['ServiceLMember']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('ServiceLimitTarget')
