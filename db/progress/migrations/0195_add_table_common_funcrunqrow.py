from gearbox.migrations import Migration

class AddTableFuncRunQRow(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunQRow', area="Sta_Data_128", label="Function Queue Row", dump_name="FuncRunQRow", desc="Row for function queue configuration")
        t.column('FRQueueID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Queue ID", column_label="Queue", position=2, order=10, help="Unique ID for the queue")
        t.column('FRQRowSeq', 'integer', format=">>>>>9", initial="0", max_width=4, label="Row Sequence", column_label="Row", position=3, order=20, help="Row sequence (order)")
        t.column('FRConfigID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="Conf.", position=4, order=30, help="Unique ID for configuration")
        t.column('MinStartTime', 'character', format="x(8)", initial="", max_width=16, label="Earliest Start Time", column_label="Min.Start", position=5, order=40, help="Earliest possible start time")
        t.column('WaitForRowSeq', 'integer', format=">>>>>9", initial="0", max_width=4, label="Wait For", column_label="Wait", position=6, order=50, help="Row which must be handled before this one is started")
        t.column('FeedFromRowSeq', 'integer', format=">>>>>9", initial="0", max_width=4, label="Feeds From", column_label="Feeds", position=7, order=60, help="Row from which feeds are taken from")
        t.index('FuncRunQRow', [['FRQueueID'], ['FRQRowSeq']], area="Sta_Index_1", primary=True, unique=True)
        t.index('FRConfigID', [['FRConfigID']], area="Sta_Index_1")

    def down(self):
        self.drop_table('FuncRunQRow')
