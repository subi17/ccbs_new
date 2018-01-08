from gearbox.migrations import Migration

class AddSequences(Migration):

    database = "common"

    def up(self):
        self.sequence('TMSRelationID', initial=1, min_val=1, cycle_on_limit="no", increment=1)

    def down(self):
        self.drop_sequence('TMSRelationID')
