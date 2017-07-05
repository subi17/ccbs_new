from gearbox.migrations import Migration

class AddFieldPROSegment(Migration):

    database = "common"
  
    def up(self):
        t = self.alter_table('CustCat')
        t.column('PRO', 'logical', format="yes/no", initial="no", label="PRO", column_label="PRO", position=15, order=140, help="PRO customer")
        t.column('Segment', 'character', format="X(20)", initial="", max_width=40, label="Segment", position=16, order=150, help="Segment")

    def down(self):
        t = self.alter_table('CustCat')
        t.drop_column('PRO')
        t.drop_column('Segment')