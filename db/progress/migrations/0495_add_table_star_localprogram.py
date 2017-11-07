from gearbox.migrations import Migration

class AddTablelocalProgram(Migration):

    database = "star"

    def up(self):
        t = self.table('localProgram', area="Sta_Data_256", dump_name="lprogram")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=2, order=10, help="Programcode")
        t.column('rightgroup', 'character', format="X(12)", initial="", max_width=24, label="RightGroup", position=3, order=20, help="Right group code")
        t.column('PrintDestinations', 'character', format="X(40)", initial="", max_width=80, label="Print destinations", position=4, order=30, help="Available print destinations")
        t.column('emailCode', 'integer', format="9", initial="0", max_width=4, label="eMail code", position=5, order=40, description='''0=all
1=self
2=to list
3=not allowed''')
        t.column('eMail', 'character', format="X(50)", initial="", max_width=100, label="eMail", position=6, order=50, help="eMail addresses. Each address to own line")
        t.column('menutooltip', 'character', format="X(256)", initial="", max_width=512, label="Tooltip", position=7, order=60, help="Program tooltip. Shows on menus... local text")
        t.column('archive', 'character', format="X(10)", initial="", help="Archive time", max_width=20, label="Archive", position=8, order=70, description='''Default
Year
Month
Week
Permanent''')
        t.index('programcode', [['programcode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('localProgram')
