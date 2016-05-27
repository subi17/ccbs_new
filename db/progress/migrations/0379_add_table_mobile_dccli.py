from gearbox.migrations import Migration

class AddTableDCCli(Migration):

    database = "mobile"

    def up(self):
        t = self.table('DCCli', area="Sta_Data_128", table_trigger=[{'crc': '?', 'procedure': 'rd-dccli.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-dccli.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="dccli")
        t.column('MSSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Sub.ID", position=3, order=20, help="Sequence for a subscription")
        t.column('ValidFrom', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=4, order=30, help="Valid from")
        t.column('ValidTo', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=5, order=40, help="Valid to")
        t.column('DCEvent', 'character', format="x(12)", initial="", max_width=24, label="Periodical Term", column_label="Term", position=6, order=50, help="ID of periodical term")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="MSISDN", column_label="CLI", position=7, order=60, help="MSISDN")
        t.column('ContractDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Contract Date", column_label="Contract", position=8, order=70, help="Date when contract was originally signed")
        t.column('TermDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Termination Date", column_label="Terminate", position=9, order=80, help="Date when contract when will be terminated")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=10, order=90, help="Code Of Brand")
        t.column('CreateFees', 'logical', format="Yes/No", initial="yes", max_width=1, label="Create Fees", column_label="Fees", position=11, order=100, help="Create fees when contract is changed or terminated")
        t.column('RenewalDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Renewal Date", column_label="Renewal", position=12, order=110, help="Date when contract was renewed")
        t.column('ValidToOrig', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Valid To Original", column_label="ValidToOrig", position=13, order=120, help="Original Valid To")
        t.column('PerContractID', 'integer', format=">>>>>>>9", initial="0", help="Periodical contract ID", max_width=4, label="Periodical Contract ID", column_label="Per.Contr.", position=14, order=130, description="unique sequence ID")
        t.column('Amount', 'decimal', format="->>>>>>9.99999", decimals=5, initial=self.unknown, max_width=20, label="Amount", column_label="Amount", position=15, order=140, help="Penalty amount")
        t.index('Contract', [['MSSeq'], ['DCEvent']], area="Sta_Index_2", primary=True)
        t.index('DCEvent', [['Brand'], ['DCEvent'], ['MSSeq'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('MSSeq', [['MSSeq'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('PerContractID', [['PerContractID', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('DCCli')
