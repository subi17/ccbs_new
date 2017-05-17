from gearbox.migrations import Migration

class AddTableMSISDNNUMBER(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('MSISDNNUMBER', area="Sta_Data_256", dump_name="msisdnnu", desc="A Single MSISDN number")
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=2, order=10, help="MSISDN Subscriber No")
        t.column('MsisdnType', 'integer', format=">9", initial="0", max_width=4, label="MSISDNType", column_label="MSISDNType", position=19, order=20, help="MSISDN Type")
        t.column('Rank', 'integer', format=">>>9", initial="0", max_width=4, label="Rank", column_label="MSISDNType", position=20, order=30, help="Rank")
        t.index('CLI', [['CLI']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('MSISDNNUMBER')
