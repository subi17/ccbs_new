from gearbox.migrations import Migration

class AddTableTMSRelation(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSRelation', area="Sta_Data_128", multitenant="yes", label="Report Texts", dump_name="tmsrel", desc="General purpose relation table")
        t.column('TMSRelationID', 'int64', format=">>>>>>>>>9", initial="0", max_width=8, label="Relation ID", column_label="ID", position=2, order=10, help="Unique relation id number")
        t.column('TableName', 'character', format="x(25)", initial="", max_width=50, label="Table name", column_label="TableName", position=3, order=20)
        t.column('KeyType', 'character', format="x(30)", initial="", max_width=60, label="Key type", column_label="KeyType", position=4, order=30)
        t.column('ParentValue', 'character', format="x(30)", initial="", max_width=60, label="Parent value", column_label="ParentValue", position=5, order=40)
        t.column('ChildValue', 'character', format="x(30)", initial="", max_width=60, label="Child value", column_label="ChildValue", position=6, order=50)
        t.column('FromTime', 'datetime-tz', format="99-99-9999 HH:MM:SS.SSS+HH:MM", initial="?", max_width=12, label="From time", column_label="FromTime", position=7, order=60)
        t.column('ToTime', 'datetime-tz', format="99-99-9999 HH:MM:SS.SSS+HH:MM", initial="?", max_width=12, label="To time", column_label="ToTime", position=8, order=70)
        t.column('RelationType', 'character', format="x(20)", initial="", max_width=40, label="RelationType", column_label="RelationType", position=9, order=80, help="Type of the relation")
        t.index('ParentValue', [['TableName'], ['KeyType'], ['ParentValue'], ['ToTime', 'DESC']], area="Sta_Index_3", primary=True)
        t.index('ChildValue', [['TableName'], ['KeyType'], ['ChildValue'], ['ParentValue'], ['ToTime', 'DESC']], area="Sta_Index_3", unique=True)
        t.index('TMSRelationID', [['TMSRelationID']], area="Sta_Index_3", unique=True)
        t.index('ToTime', [['TableName'], ['KeyType'], ['ToTime', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('TMSRelation')
