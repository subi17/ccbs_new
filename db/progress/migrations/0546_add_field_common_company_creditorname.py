from gearbox.migrations import Migration

class AddFieldCreditorName(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Company')
        t.column('CreditorName', 'character', format="X(30)", initial="", label="Creditor Name", column_label="CreditorName", position=17, order=520, help="Creditor Name in DD file")

    def down(self):
        t = self.alter_table('Company')
        t.drop_column('CreditorName')
