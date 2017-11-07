from gearbox.migrations import Migration

class AddTablestarfind(Migration):

    database = "star"

    def up(self):
        t = self.table('starfind', area="Sta_Data_256", label="Data to starFind rutine", dump_name="starfind")
        t.column('queryname', 'character', format="X(15)", initial="", max_width=30, label="Query", position=2, order=10, help="Name of query. Exsample CustomerName")
        t.column('tablename', 'character', format="X(15)", initial="", max_width=30, label="Table", position=3, order=20, help="Query table name")
        t.column('fieldname', 'character', mandatory=True, format="X(40)", initial="", max_width=80, label="Field", position=4, order=30, help="Key field names of query. Separed by comma")
        t.column('displayfield', 'character', mandatory=True, format="X(15)", initial="", max_width=30, label="Display", position=5, order=40, help="Field name of displayfield")
        t.index('querybane', [['queryname']], area="Sta_Index_2", primary=True, unique=True)
        t.index('tablename', [['tablename']], area="Sta_Index_2")

    def down(self):
        self.drop_table('starfind')
