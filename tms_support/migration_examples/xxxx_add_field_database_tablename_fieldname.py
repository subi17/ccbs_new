from gearbox.migrations import Migration

class AddFieldFieldName(Migration):

    database = "database"

    def up(self):
        t = self.alter_table('TableName')
        t.column('FieldName', 'character', format="X(30)", initial="", label="FieldName", column_label="FieldName", position=17, order=520, help="FieldName")

    def down(self):
        t = self.alter_table('TableName')
        t.drop_column('FieldName')
