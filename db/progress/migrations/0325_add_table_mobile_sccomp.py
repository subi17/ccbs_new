from gearbox.migrations import Migration

class AddTableScComp(Migration):

    database = "mobile"

    def up(self):
        t = self.table('ScComp', area="Sta_Data_256", label="ScComp", dump_name="sccomp", desc='''
''')
        t.column('ScCode2', 'character', format="x(12)", initial="", max_width=24, label="ComparedWith", column_label="ComparedWith", position=2, order=10, help="Compared With")
        t.column('ScCondition', 'logical', format="Required/Prohibited", initial="no", max_width=1, label="Condition", column_label="Condition", position=3, order=20, help="What is Condition of combination ?")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=4, order=30, help="Explanation / memory field for Service Combination")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Comparable", column_label="Comparable", position=5, order=40, help="Comparable")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=6, order=50, help="Code Of Brand")
        t.index('ServCom', [['Brand'], ['ServCom'], ['ScCode2']], area="Sta_Index_3", primary=True)
        t.index('ScCode2', [['Brand'], ['ScCode2'], ['ServCom']], area="Sta_Index_3")

    def down(self):
        self.drop_table('ScComp')
