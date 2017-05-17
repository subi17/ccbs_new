from gearbox.migrations import Migration

class AddSequences(Migration):

    database = "ordercanal"

    def up(self):
        self.sequence('MobSub', initial=1, min_val=1, max_val=99999999, cycle_on_limit="no", increment=1)
        self.sequence('OrderId', initial=1, min_val=1, cycle_on_limit="no", increment=1)
        self.sequence('CallAlarm', initial=1, min_val=1, cycle_on_limit="no", increment=1)
        self.sequence('PrePaidReq', initial=0, min_val=0, cycle_on_limit="no", increment=1)
        self.sequence('PPRequest', initial=0, min_val=0, cycle_on_limit="no", increment=1)
        self.sequence('SMSSeq', initial=0, min_val=0, cycle_on_limit="no", increment=1)
        self.sequence('DMS', initial=1, min_val=1, cycle_on_limit="no", increment=1)
        self.sequence('FusionMessageSeq', initial=0, min_val=0, cycle_on_limit="no", increment=1)

    def down(self):
        self.drop_sequence('FusionMessageSeq')
        self.drop_sequence('DMS')
        self.drop_sequence('SMSSeq')
        self.drop_sequence('PPRequest')
        self.drop_sequence('PrePaidReq')
        self.drop_sequence('CallAlarm')
        self.drop_sequence('OrderId')
        self.drop_sequence('MobSub')
