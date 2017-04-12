from gearbox.migrations import Migration

class AddFieldLanguageCode(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Language')
        t.column('LanguageCode', 'character', format="x(2)", initial="", max_width=4, label="LanguageCode", column_label="LangCode", position=4, order=30, help="ISO 639-1 code for the language.")

    def down(self):
        t = self.alter_table('Language')
        t.drop_column('LanguageCode')
