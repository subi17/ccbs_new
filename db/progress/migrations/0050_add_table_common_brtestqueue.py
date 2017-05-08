from gearbox.migrations import Migration

class AddTableBRTestQueue(Migration):

    database = "common"

    def up(self):
        t = self.table('BRTestQueue', area="Sta_Data_2_256", label="BR Test Queue", dump_name="brtestqueue", desc="Billrun test queue")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('BRTestQueueID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Test Queue ID", column_label="Queue ID", position=3, order=20, help="Billrun test queue ID")
        t.column('Description', 'character', format="x(50)", initial="", max_width=100, label="Description", position=4, order=30, help="Description of test queue")
        t.column('Active', 'logical', format="Yes/No", initial="no", max_width=1, label="Active", position=5, order=40, help="Is queue active")
        t.column('ResultEMail', 'character', format="x(40)", initial="", max_width=80, label="Result eMail", column_label="eMail", position=6, order=50, help="eMail to which results are sent")
        t.index('BRTestQueueID', [['BRTestQueueID']], area="Sta_Index_4", primary=True, unique=True)
        t.index('Description', [['Brand'], ['Description']], area="Sta_Index_4")

    def down(self):
        self.drop_table('BRTestQueue')
