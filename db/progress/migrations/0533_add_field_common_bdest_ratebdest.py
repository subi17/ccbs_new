from gearbox.migrations import Migration

class AddFieldRateBDest(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BDest')
        t.column('RateBDest', 'character', format="X(25)", initial="", label="Rating Destination", column_label="RateBDest", position=23, order=230, help="Rating B-destination")

    def down(self):
        t = self.alter_table('BDest')
        t.drop_column('RateBDest')
