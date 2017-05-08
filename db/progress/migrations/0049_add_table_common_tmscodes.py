from gearbox.migrations import Migration

class AddTableTMSCodes(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSCodes', area="Sta_Data_64", label="Selection Lists", dump_name="tmscodes", desc="General selection list values in TMS application")
        t.column('TableName', 'character', mandatory=True, format="X(15)", initial="", help="Database Table Name", max_width=30, label="Table Name", column_label="Table Name", position=2, order=10, description="DB table, field of which is using pre-defined values.")
        t.column('FieldName', 'character', mandatory=True, format="X(20)", initial="", help="Field Name in the table", max_width=40, label="Field Name", column_label="FieldName", position=3, order=20, description="Name of the Field in the table which is using pre-defined values.")
        t.column('CodeGroup', 'character', format="X(10)", initial="", help="Group Name for pre-defined values", max_width=20, label="Group", column_label="Group", position=4, order=30, description="Group in which pre-defined values can be categorised.")
        t.column('CodeValue', 'character', format="X(8)", initial="", max_width=16, label="Value", column_label="Value", position=5, order=40, help="Possible pre-defined value")
        t.column('CodeName', 'character', format="X(50)", initial="", max_width=100, label="Description", column_label="Description", position=6, order=50, help="Description/Name for pre-defined value")
        t.column('Memo', 'character', format="X(60)", initial="", max_width=120, label="Memo", column_label="Memo", position=7, order=60, help="Detailed description for pre-defined values")
        t.column('ConfigValue', 'character', format="x(20)", initial="", max_width=40, label="Configuration Value", column_label="Config", position=8, order=70, help="Configuration value")
        t.column('InUse', 'integer', format="9", initial="0", max_width=4, label="Code In Use", column_label="Used", position=9, order=80, help="Is code in use")
        t.index('TableName', [['TableName'], ['FieldName'], ['CodeValue']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CodeGroup', [['CodeGroup'], ['FieldName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TMSCodes')
