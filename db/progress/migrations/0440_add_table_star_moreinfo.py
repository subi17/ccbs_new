from gearbox.migrations import Migration

class AddTablemoreinfo(Migration):

    database = "star"

    def up(self):
        t = self.table('moreinfo', area="Sta_Data_256", label="MoreInfo", dump_name="moreinfo", desc="Moreinfo menus")
        t.column('name', 'character', format="X(40)", initial="", max_width=80, label="Name", position=2, order=30, help="Program name. Use menu labels and program title")
        t.column('number', 'integer', format=">9", initial="0", max_width=4, label="Number", position=3, order=20, help="Number for moreinfo")
        t.column('makemenus', 'logical', format="yes/no", initial="no", max_width=1, label="Make menus", position=4, order=40, help="Make menus")
        t.column('makebuttons', 'logical', format="yes/no", initial="yes", max_width=1, label="Make buttons", position=5, order=50, help="Make buttons")
        t.column('programcode', 'character', format="X(12)", initial="", max_width=24, label="Programcode", position=6, order=10, help="Programcode")
        t.index('moreinfo', [['programcode'], ['number']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('moreinfo')
