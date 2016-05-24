from gearbox.migrations import Migration

class AddTableSubsTerminal(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('SubsTerminal', area="Sta_Data_64", label="Subscription Terminal", table_trigger=[{'crc': '?', 'procedure': 'rd-substerminal.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-substerminal.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="substerminal", desc="Terminal of a subscription")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('MSSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subs.ID", position=3, order=20, help="Subscription ID")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Order ID", column_label="Order", position=4, order=30, help="Order ID")
        t.column('TerminalID', 'integer', format=">>>>>>>>>9", initial="0", help="Terminal ID", max_width=4, label="Terminal ID", column_label="Term.ID", position=5, order=40, description="unique sequence nbr")
        t.column('IMEI', 'character', format="x(15)", initial="", max_width=30, label="IMEI", column_label="IMEI", position=6, order=50, help="IMEI code")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="Bill.Item", position=7, order=60, help="Billing item code")
        t.column('PurchaseTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Purchased", position=8, order=70, help="Time of purchase")
        t.column('Manufacturer', 'character', format="x(30)", initial="", max_width=60, label="Manufacturer", column_label="Manufacturer", position=9, order=110, help="Terminal manufacturer")
        t.column('Model', 'character', format="x(30)", initial="", max_width=60, label="Model", column_label="Model", position=10, order=90, help="Terminal model")
        t.column('ModelColor', 'character', format="x(30)", initial="", max_width=60, label="ModelColor", column_label="ModelColor", position=11, order=100, help="Terminal color")
        t.column('SIMLockCode', 'character', format="x(20)", initial="", max_width=40, label="SIM Lock Code", position=12, order=120, help="SIM lock code")
        t.column('PerContractID', 'integer', format=">>>>>>>>>9", initial="0", help="Periodical contract ID", max_width=4, label="Periodical Contract ID", column_label="Per.Contr.", position=13, order=130, description="DCCLI")
        t.column('SIMChecked', 'logical', format="Yes/No", initial="no", max_width=1, label="SIM Checked", column_label="SIMCheck", position=14, order=140, help="SIM checked")
        t.column('TerminalType', 'integer', format=">9", initial="0", max_width=4, label="Terminal Type", column_label="Term.Type", position=15, order=150, help="Type of terminal")
        t.index('TerminalID', [['TerminalID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('MsSeq', [['MSSeq'], ['PurchaseTS', 'DESC']], area="Sta_Index_1")
        t.index('OrderId', [['Brand'], ['OrderId']], area="Sta_Index_1")

    def down(self):
        self.drop_table('SubsTerminal')
