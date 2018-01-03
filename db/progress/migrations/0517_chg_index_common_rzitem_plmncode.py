from gearbox.migrations import Migration

class ChgIndexPLMNCode(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('RZItem')
        t.alter_index('PLMNCode', [['PlmnCode']], area="Sta_Index_2", primary=True)

    def down(self):
        t = self.alter_table('RZItem')
        t.alter_index('PLMNCode', [['PlmnCode']], area="Sta_Index_2", primary=True, unique=True)
