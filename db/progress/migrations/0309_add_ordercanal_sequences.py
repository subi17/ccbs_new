from gearbox.migrations import Migration

class AddordercanalSequences(Migration):

    dumped_on = "propus"
    database = "ordercanal"

    def up(self):
        self.sequence("MobSub", initial=1, min_val=1, max_val=99999999)
        self.sequence("OrderId", initial=1, min_val=1)
        self.sequence("CallAlarm", initial=1, min_val=1)
        self.sequence("PrePaidReq")
        self.sequence("PPRequest")


    def down(self):
        self.drop_sequence("PPRequest")
        self.drop_sequence("PrePaidReq")
        self.drop_sequence("CallAlarm")
        self.drop_sequence("OrderId")
        self.drop_sequence("MobSub")

