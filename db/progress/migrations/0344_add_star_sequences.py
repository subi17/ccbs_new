from gearbox.migrations import Migration

class AddstarSequences(Migration):

    dumped_on = "propus"
    database = "star"

    def up(self):
        self.sequence("r-codeseq")
        self.sequence("progjob")
        self.sequence("JobId", max_val=2000000000, cycle_on_limit=True)
        self.sequence("sessionId")
        self.sequence("CLISeries")


    def down(self):
        self.drop_sequence("CLISeries")
        self.drop_sequence("sessionId")
        self.drop_sequence("JobId")
        self.drop_sequence("progjob")
        self.drop_sequence("r-codeseq")

