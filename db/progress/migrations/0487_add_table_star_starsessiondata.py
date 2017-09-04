from gearbox.migrations import Migration

class AddTableStarSessionData(Migration):

    database = "star"

    def up(self):
        t = self.table('StarSessionData', area="Sta_Data_256", dump_name="starsesd")
        t.column('SessionId', 'integer', format="->,>>>,>>>,>>9", initial="0", max_width=4, label="SessionId", column_label="Id", position=2, order=10, help="SessionId")
        t.column('programname', 'character', format="X(50)", initial="", max_width=100, label="Program name", position=3, order=20, help="Program name")
        t.column('data', 'character', format="X(78)", initial="", max_width=156, label="Data", position=5, order=30, help="Values using get/setStarProperty method")
        t.index('ProgramName', [['SessionId'], ['programname']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('StarSessionData')
