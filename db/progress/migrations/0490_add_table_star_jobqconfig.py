from gearbox.migrations import Migration

class AddTablejobQConfig(Migration):

    database = "star"

    def up(self):
        t = self.table('jobQConfig', area="Sta_Data_256", dump_name="jobqconf")
        t.column('queueNumber', 'integer', format=">>9", initial="0", max_width=4, label="Queue number", position=2, order=150, help="1...999 queues")
        t.column('queuestatus', 'logical', format="Active/Stopped", initial="Active", max_width=1, label="Active", position=3, order=160)
        t.column('queueType', 'character', format="X(8)", initial="", max_width=16, label="Type", position=4, order=170, help="Queue type")
        t.index('Queue', [['queueNumber']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('jobQConfig')
