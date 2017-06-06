from gearbox.migrations import Migration

class AddSequences(Migration):

    database = "counter"

    def up(self):
        self.sequence('TriggerEvent', initial=1, min_val=1, cycle_on_limit="no", increment=1)

    def down(self):
        self.drop_sequence('TriggerEvent')
