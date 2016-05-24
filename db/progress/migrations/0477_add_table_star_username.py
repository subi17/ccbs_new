from gearbox.migrations import Migration

class AddTableusername(Migration):

    database = "star"

    def up(self):
        t = self.table('username', area="Sta_Data_256", label="Username", dump_name="username")
        t.column('usercode', 'character', format="X(8)", initial="", max_width=16, label="User", position=2, order=10, help="User code")
        t.column('name', 'character', mandatory=True, format="X(50)", initial="", max_width=100, label="Name", position=3, order=20, help="User realname")
        t.column('eMail', 'character', format="X(50)", initial="", max_width=100, label="eMail", position=4, order=30, help="eMail address")
        t.column('lastname', 'character', format="X(25)", initial="", max_width=50, label="Last Name", position=5, order=40, help="Person lastname")
        t.column('firstname', 'character', format="X(25)", initial="", max_width=50, label="Frist name", position=6, order=50, help="Person first name")
        t.column('password', 'character', format="X(20)", initial="", max_width=40, position=7, order=60, help="Password. It stored ecrypted to database")
        t.column('helpEditor', 'logical', format="yes/no", initial="no", max_width=1, label="Help Editor", position=9, order=80, help="Help Editor")
        t.column('menulanguage', 'character', format="X(8)", initial="eng", max_width=16, label="Menu language", position=10, order=90, help="User's menu language")
        t.column('usergroups', 'character', format="X(50)", initial="", max_width=80, label="UserGroups", position=11, order=100, help="Comma separed list of user groups")
        t.column('durationbegin', 'date', format="99.99.99", max_width=4, label="Begin", position=12, order=110, help="Duration begin")
        t.column('durationend', 'date', format="99.99.99", max_width=4, label="End", position=13, order=120, help="Duration end")
        t.index('usercode', [['usercode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('username', [['name']], area="Sta_Index_2")

    def down(self):
        self.drop_table('username')
