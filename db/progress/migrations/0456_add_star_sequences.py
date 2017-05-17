from gearbox.migrations import Migration

class AddSequences(Migration):

    database = "star"

    def up(self):
        self.sequence('r-codeseq', initial=0, min_val=0, cycle_on_limit="no", increment=1)
        self.sequence('progjob', initial=0, min_val=0, cycle_on_limit="no", increment=1)
        self.sequence('JobId', initial=0, min_val=0, max_val=2000000000, cycle_on_limit="yes", increment=1)
        self.sequence('sessionId', initial=0, min_val=0, cycle_on_limit="no", increment=1)
        self.sequence('CLISeries', initial=0, min_val=0, cycle_on_limit="no", increment=1)

    def down(self):
        self.drop_sequence('CLISeries')
        self.drop_sequence('sessionId')
        self.drop_sequence('JobId')
        self.drop_sequence('progjob')
        self.drop_sequence('r-codeseq')
