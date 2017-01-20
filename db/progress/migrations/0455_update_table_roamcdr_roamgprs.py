from gearbox.migrations import Migration

class ChgTableRoamGPRS(Migration):

    database = "roamcdr"

    def up(self):
        t = self.alter_table('RoamGPRS')
        t.alter_indexdata('PartialRec', buffer_pool="Alternate")

    def down(self):
        self.alter_table('RoamGPRS')
        t.alter_indexdata(PartialRec)
