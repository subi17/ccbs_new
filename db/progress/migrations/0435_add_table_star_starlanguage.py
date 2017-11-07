from gearbox.migrations import Migration

class AddTablestarLanguage(Migration):

    database = "star"

    def up(self):
        t = self.table('starLanguage', area="Sta_Data_256", dump_name="starlang")
        t.column('translategroup', 'character', format="X(15)", initial="", max_width=30, label="Group", position=2, order=10, help="Program,Menu,Label,Button,Table")
        t.column('language', 'character', format="X(8)", initial="", max_width=16, label="Language", position=3, order=50, help="eng,fin,swe")
        t.column('key1', 'character', format="X(50)", initial="", max_width=100, label="Key 1", position=4, order=20, help="Key 1")
        t.column('name', 'character', format="X(30)", initial="", max_width=60, label="Name", position=5, order=60, help="Name in current language")
        t.column('helptxt', 'character', format="X(50)", initial="", max_width=100, label="Help", position=6, order=70, help="Help text in current language")
        t.column('fieldformat', 'character', format="X(20)", initial="", max_width=40, label="Format", position=7, order=80, help="Field format")
        t.column('key2', 'character', format="X(50)", initial="", max_width=100, label="Key 2", position=8, order=30, help="Key 2")
        t.column('key3', 'character', format="X(50)", initial="", max_width=100, label="Key 3", position=9, order=40, help="Key 3")
        t.index('main', [['translategroup'], ['key1'], ['key2'], ['key3'], ['language']], area="Sta_Index_2", primary=True, unique=True)
        t.index('language', [['language'], ['translategroup'], ['key1'], ['key2'], ['key3']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('starLanguage')
