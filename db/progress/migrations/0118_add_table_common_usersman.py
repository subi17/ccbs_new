from gearbox.migrations import Migration

class AddTableUserSman(Migration):

    database = "common"

    def up(self):
        t = self.table('UserSman', area="Sta_Data_256", label="UserSman", dump_name="usersman", desc='''User's salesman definitions
''')
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="User ID", column_label="User ID", position=2, order=10, help="User ID, 1 - 8 characters")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=3, order=20, help="Code Of Brand")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=4, order=30, help="Salesman's code")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", position=5, order=40, help="Information")
        t.index('UserCode', [['UserCode'], ['Brand']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Salesman', [['Salesman']], area="Sta_Index_2")

    def down(self):
        self.drop_table('UserSman')
