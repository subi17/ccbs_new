from gearbox.migrations import Migration

class AddTableRepLog(Migration):

    database = "mobile"

    def up(self):
        t = self.table('RepLog', area="CDF_Data", dump_name="replog", desc="This is the replication log table for replicating the record")
        t.column('RowID', 'character', format="x(18)", initial="", max_width=36, label="RowID", column_label="RowID", position=2, order=10, description="RowID in character format")
        t.column('KeyValue', 'character', format="x(30)", initial="", max_width=60, label="KeyValue", column_label="KeyValue", position=3, order=20)
        t.column('TableName', 'character', format="x(15)", initial="", max_width=30, label="Table name", column_label="TableName", position=4, order=30)
        t.column('EventType', 'character', format="x(6)", initial="", max_width=12, label="Event type", column_label="EventType", position=5, order=40)
        t.column('EventTime', 'datetime-tz', format="99-99-9999 HH:MM:SS.SSS+HH:MM", initial="?", max_width=12, label="Event time", column_label="EventTime", position=6, order=50)
        t.column('SendTime', 'datetime-tz', format="99-99-9999 HH:MM:SS.SSS+HH:MM", initial="?", max_width=12, label="Send time", column_label="SendTime", position=7, order=60)
        t.column('SendCount', 'integer', format=">>>9", initial="0", max_width=4, label="Send count", column_label="SendCount", position=8, order=70)
        t.index('SendTime', [['SendTime'], ['EventTime']], area="CDF_Index", primary=True)

    def down(self):
        self.drop_table('RepLog')
