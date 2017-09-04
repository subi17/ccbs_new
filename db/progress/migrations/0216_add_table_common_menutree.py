from gearbox.migrations import Migration

class AddTableMenuTree(Migration):

    database = "common"

    def up(self):
        t = self.table('MenuTree', area="Sta_Data_64", label="Menu Tree", dump_name="menutree", desc="Menu configuration, menu tree")
        t.column('Level', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Level", column_label="Level", position=2, order=10, help="Menu level (main level 0, sublevel f.ex. 134)")
        t.column('Position', 'integer', mandatory=True, valexp="Position < 9", format="9", initial="0", max_width=4, label="P", column_label="P", position=3, order=20, valmsg="Has to be 0 ... 8 !", help="Menu slot (1  ...  8), 0 if not a MENU item")
        t.column('MenuNum', 'integer', mandatory=True, format=">>9", initial="0", max_width=4, label="Mno", column_label="Mno", position=4, order=30, help="Menutext's number")
        t.column('MenuType', 'integer', mandatory=True, valexp="MenuType > 0 and MenuType < 4", format="9", initial="1", max_width=4, label="T", column_label="T", position=5, order=40, valmsg="Has to be 1 ... 3 !", help="Leads to: 1: program, 2: next menu, 3: previous menu")
        t.column('Module', 'character', mandatory=True, format="x(10)", initial="", max_width=20, label="Module", column_label="Module", position=6, order=50, help="Name of called program or the next menu level")
        t.column('MenuId', 'character', format="x(8)", initial="", max_width=16, label="Fcode", column_label="Fcode", position=7, order=60, help="Function code")
        t.column('MenuTitle', 'character', format="x(40)", initial="", max_width=80, label="Fname", column_label="Fname", position=8, order=70, help="Function name")
        t.column('HotKey', 'character', format="x(8)", initial="", max_width=16, label="Shortcut", column_label="Shortcut", position=9, order=80, help="Function's shortcut")
        t.column('UserRight', 'integer', format="ZZ9", initial="0", max_width=4, label="Privil", column_label="Privil", position=10, order=90, help="Function privilege")
        t.column('Memo', 'character', format="x(75)", initial="", max_width=4864, label="Info", column_label="Info", extent=32, position=11, order=100, help="Description, information and hints for use")
        t.column('State', 'logical', format="Yes/No", initial="no", max_width=24, label="Status", column_label="Status", extent=3, position=12, order=111, help="Status code")
        t.column('MenuClass', 'integer', format="zzz9", initial="0", max_width=4, label="PgCl", column_label="PgCl", position=13, order=110, help="Unique number for Program Class")
        t.column('TokenCode', 'character', format="X(12)", initial="", max_width=24, label="Token", position=14, order=121, help="Comma separed list of Token codes")
        t.index('Level', [['Level'], ['Position']], area="Sta_Index_2", primary=True)
        t.index('HotKey', [['HotKey']], area="Sta_Index_2")
        t.index('MenuClass', [['MenuClass'], ['MenuId']], area="Sta_Index_2")
        t.index('MenuId', [['MenuId']], area="Sta_Index_2")
        t.index('MenuTitle', [['MenuTitle'], ['MenuId']], area="Sta_Index_2")
        t.index('Module', [['Module'], ['Level'], ['Position']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MenuTree')
