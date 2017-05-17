from gearbox.migrations import Migration

class AddTableFuncRunQueue(Migration):

    database = "common"

    def up(self):
        t = self.table('FuncRunQueue', area="Sta_Data_128", label="Function Queue", dump_name="FuncRunQueue", desc="Configuration for function queue")
        t.column('FRQueueID', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Queue ID", column_label="Queue", position=2, order=10, help="Unique ID for the queue")
        t.column('QueueDesc', 'character', format="x(30)", initial="", max_width=60, label="Description", position=3, order=20, help="Short description of the queue")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=4, order=30, help="Code of brand")
        t.column('Active', 'logical', format="Yes/No", initial="no", max_width=1, label="Active", position=5, order=40, help="Is queue active")
        t.index('FRQueueID', [['FRQueueID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('BrandQueue', [['Brand'], ['FRQueueID']], area="Sta_Index_1")

    def down(self):
        self.drop_table('FuncRunQueue')
