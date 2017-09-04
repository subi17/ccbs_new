from gearbox.migrations import Migration

class AddSequences(Migration):

    database = "mobile"

    def up(self):
        self.sequence('RoamTariff', initial=1, min_val=0, max_val=999999, cycle_on_limit="no", increment=1)

    def down(self):
        self.drop_sequence('RoamTariff')
