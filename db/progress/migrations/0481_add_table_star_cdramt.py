from gearbox.migrations import Migration

class AddTableCDRAmt(Migration):

    database = "star"

    def up(self):
        t = self.table('CDRAmt', area="Sta_Data_256", label="CDRAmt", dump_name="cdramt", desc="Summary of read CDRs for OnLine displaying")
        t.column('Date', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=2, order=10, help="Date of the counter")
        t.column('ExCode', 'character', format="x(8)", initial="", max_width=16, label="Switch", column_label="Switch", position=3, order=20, help="Calls from switch")
        t.column('QtyTot', 'integer', format=">,>>>,>>9", initial="0", max_width=480, label="Amt", column_label="Amt", extent=24, position=4, order=50, help="Amount of calls per hour")
        t.column('QtyDB', 'integer', format=">,>>>,>>9", initial="0", max_width=480, label="#DB", column_label="#DB", extent=24, position=5, order=60, help="Amount of calls per hour saved in the database")
        t.column('QtyJunk', 'integer', format=">,>>>,>>9", initial="0", max_width=480, label="#Junk", column_label="#Junk", extent=24, position=6, order=70, help="Amount of calls per hour dumped into junk")
        t.index('ExCode', [['ExCode'], ['Date']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('CDRAmt')
