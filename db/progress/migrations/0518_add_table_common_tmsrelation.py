from gearbox.migrations import Migration

class AddTableTMSRelation(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSRelation', area="Sta_Data_128", multitenant="yes", label="Report Texts", dump_name="tmsrel", desc="General purpose relation table")
        t.column('TableName', 'character', format="x(25)", initial="", max_width=50, label="Table name", column_label="TableName", position=2, order=10)
        t.column('KeyType', 'character', format="x(30)", initial="", max_width=60, label="Key type", column_label="KeyType", position=3, order=20)
        t.column('KeyValue1', 'character', format="x(30)", initial="", max_width=60, label="KeyValue1", column_label="KeyValue1", position=4, order=30)
        t.column('KeyValue2', 'character', format="x(30)", initial="", max_width=60, label="KeyValue2", column_label="KeyValue2", position=5, order=40)
        t.column('FromTime', 'datetime-tz', format="99-99-9999 HH:MM:SS.SSS+HH:MM", initial="?", max_width=12, label="From time", column_label="FromTime", position=6, order=50)
        t.column('ToTime', 'datetime-tz', format="99-99-9999 HH:MM:SS.SSS+HH:MM", initial="?", max_width=12, label="To time", column_label="ToTime", position=7, order=60)
        t.column('RelationType', 'character', format="x(20)", initial="", max_width=40, label="RelationType", column_label="RelationType", position=8, order=70, help="Type of the relation")
        t.index('TableName', [['TableName'], ['KeyType'], ['KeyValue1'], ['ToTime', 'DESC']], area="Sta_Index_3", primary=True)
        t.index('TableName', [['TableName'], ['KeyType'], ['KeyValue2'], ['KeyValue1'], ['ToTime', 'DESC']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('TMSRelation')
