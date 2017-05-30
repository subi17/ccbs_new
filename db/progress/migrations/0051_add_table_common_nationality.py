from gearbox.migrations import Migration

class AddTableNationality(Migration):

    database = "common"

    def up(self):
        t = self.table('Nationality', area="Sta_Data_128", label="Nationalities", dump_name="national", desc='''Nationalities

''')
        t.column('Nationality', 'character', format="x(8)", initial="", help="Nationality", max_width=16, label="Nationality", column_label="Nation.", position=2, order=10, description='''

''')
        t.column('NtName', 'character', format="x(30)", initial="", max_width=60, label="Description", position=3, order=20, help="Description")
        t.column('TFNationality', 'character', format="x(5)", initial="", max_width=10, label="TFNationality", position=4, order=30, help="TFNationality")
        t.index('Nationality', [['Nationality']], area="Sta_Index_2", primary=True, unique=True)
        t.index('NtName', [['NtName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Nationality')
