from gearbox.migrations import Migration

class AddTableAuthUser(Migration):

    database = "common"

    def up(self):
        t = self.table('AuthUser', area="Sta_Data_64", label="AuthUser", dump_name="authuser", desc="3rd party user authentication")
        t.column('Username', 'character', format="x(20)", initial="", max_width=40, label="Username", column_label="Username", position=2, order=10, description="the login name")
        t.column('PWHash', 'raw', format="x(40)", initial="", max_width=80, label="PWHash", column_label="PWHash", position=3, order=20, description="the hash (SHA-1) of of the user's password")
        t.column('PWSalt', 'raw', format="x(40)", initial="", max_width=80, label="PWSalt", column_label="the hashing salt", position=4, order=30)
        t.index('Username', [['Username']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('AuthUser')
