from gearbox.migrations import Migration

class AddmobileSequences(Migration):

    dumped_on = "propus"
    database = "mobile"

    def up(self):
        self.sequence("RoamTariff", initial=1, max_val=999999)


    def down(self):
        self.drop_sequence("RoamTariff")

