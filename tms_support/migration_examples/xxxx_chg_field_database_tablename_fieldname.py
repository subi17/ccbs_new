from gearbox.migrations import Migration

class ChgFieldField(Migration):

    database = "database"

    def up(self):
        t = self.alter_table('tablename')
        t.rename_column('old_fieldname', 'fieldname')
        t.alter_column('fieldname', format="x(11)", label="fieldname", column_label="fieldname", help="Field name help")

    def down(self):
        t = self.alter_table('tablename')
        t.rename_column('fieldname', 'old_fieldname')
        t.alter_column('old_fieldname', format="x(8)", label="old_fieldname", column_label="old_fieldname", help="")
