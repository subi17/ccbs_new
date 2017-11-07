from gearbox.migrations import Migration

class AddTableLanguage(Migration):

    database = "common"

    def up(self):
        t = self.table('Language', area="Sta_Data_2_256", label="Language", dump_name="language", desc="Language")
        t.column('Language', 'integer', format=">9", initial="0", max_width=4, label="Language", column_label="Language", position=2, order=10, help="Code of Language")
        t.column('LangName', 'character', format="x(35)", initial="", max_width=70, label="LangName", column_label="LangName", position=3, order=20, help="Name of Language")
        t.index('Language', [['Language']], area="Sta_Index_4", primary=True, unique=True)
        t.index('LangName', [['LangName'], ['Language']], area="Sta_Index_4", unique=True)

    def down(self):
        self.drop_table('Language')
