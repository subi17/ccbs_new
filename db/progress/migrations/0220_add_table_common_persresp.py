from gearbox.migrations import Migration

class AddTablePersResp(Migration):

    database = "common"

    def up(self):
        t = self.table('PersResp', area="Sta_Data_256", label="Responsible Personnel", dump_name="persresp", desc="Alternate Personnels responsible for handling the troubles in case main personnel is not available")
        t.column('PersCode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Person", column_label="Person", position=2, order=10, help="Person")
        t.column('Level', 'character', format="X(10)", initial="", help="Level of the alternate responsible person", max_width=20, label="Level", column_label="Level", position=3, order=20, description="Level of the alternate responsible person")
        t.column('PersKey', 'character', format="X(8)", initial="", help="Value of Person, Category or Type depending on level", max_width=16, label="Key", column_label="Key", position=4, order=30, description="Value of Person, Category or Type depending on level")
        t.column('Memo', 'character', format="X(60)", initial="", help="verbal explanation if needed", max_width=120, label="Memo", column_label="Memo", position=5, order=40, description="verbal explanation if needed")
        t.column('Priority', 'integer', format="9", initial="0", help="Priority (1-9)", max_width=4, label="Priority", column_label="Priority", position=6, order=50, description="Priority (1-9)")
        t.index('perslvlkey', [['PersCode'], ['Level'], ['PersKey']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('PersResp')
