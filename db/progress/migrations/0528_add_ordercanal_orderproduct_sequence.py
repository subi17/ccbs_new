from gearbox.migrations import Migration

class AddSequences(Migration):

    database = "ordercanal"

    def up(self):
        self.sequence('OrderProductID', initial=0, min_val=0, cycle_on_limit="no", increment=1)

    def down(self):
        self.drop_sequence('OrderProductID')
