from gearbox.migrations import Migration

class AddSequences(Migration):

    database = "ordercanal"

    def up(self):
        self.sequence('OfferCriteriaSeq', initial=1, min_val=1, cycle_on_limit="no", increment=1)
        self.sequence('OfferItemSeq', initial=1, min_val=1, cycle_on_limit="no", increment=1)

    def down(self):
        self.drop_sequence('OfferCriteriaSeq')
        self.drop_sequence('OfferItemSeq')
