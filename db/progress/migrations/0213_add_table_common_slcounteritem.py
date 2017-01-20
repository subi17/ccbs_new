from gearbox.migrations import Migration

class AddTableSLCounterItem(Migration):

    database = "common"

    def up(self):
        t = self.table('SLCounterItem', area="Sta_Data_128", dump_name="slcounteritem", desc="Keep a count of items for ServiceLCounter")
        t.column('SLSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="SLSeq", column_label="SLseq", position=2, order=10, help="Sequence for Servicelimit")
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="MsSeq", column_label="MsSeq", position=3, order=20, help="Link to mobsub-table")
        t.column('Period', 'integer', format="999999", initial="0", max_width=4, label="Period", position=4, order=30, help="Period what Servicecounter is to be")
        t.column('SLCItem', 'character', format="x(15)", initial="", max_width=30, label="SLCItem", position=5, order=40)
        t.index('msseq', [['MsSeq'], ['Period', 'DESC'], ['SLSeq'], ['SLCItem']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('SLCounterItem')
