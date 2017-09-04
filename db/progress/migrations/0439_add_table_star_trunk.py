from gearbox.migrations import Migration

class AddTableTrunk(Migration):

    database = "star"

    def up(self):
        t = self.table('Trunk', area="Sta_Data_256", label="Trunk", dump_name="trunk", desc="Trunk groups in each exchange")
        t.column('ExCode', 'character', format="x(8)", initial="", max_width=16, label="Switch", column_label="Switch", position=2, order=10, help="Swicth code")
        t.column('TrunkCode', 'character', format="x(7)", initial="", max_width=14, label="Trunk Code", column_label="Trunk Code", position=3, order=20, help="Trunk group's code")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=4, order=30, help="Memo")
        t.column('TrunkName', 'character', format="x(40)", initial="", max_width=80, label="Trunk name", column_label="Trunk name", position=5, order=15, help="Name of a Circuit Group Route")
        t.column('OpCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="Operator", column_label="Operator", position=6, order=40, help="Operator code, 1 - 8 characters")
        t.column('TrInternat', 'logical', format="Int/Nat", initial="No", max_width=1, label="Int.", column_label="Int.", position=7, order=50, help="International / National traffic")
        t.column('TrunkGroup', 'character', format="x(16)", initial="", max_width=32, label="Group", column_label="Group", position=9, order=70, help="GROUP Name")
        t.column('TrIn', 'logical', format="Out/In", initial="Yes", max_width=1, label="Conn.", column_label="Conn.", position=10, order=60, help="In / Out traffic")
        t.index('ExCode', [['ExCode'], ['TrunkGroup'], ['TrunkCode']], area="Sta_Index_2", primary=True)
        t.index('Operator', [['OpCode'], ['ExCode'], ['TrunkGroup'], ['TrunkCode']], area="Sta_Index_2")
        t.index('TrunkCode', [['TrunkCode'], ['ExCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Trunk')
