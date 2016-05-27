from gearbox.migrations import Migration

class AddTablePaymConfig(Migration):

    database = "common"

    def up(self):
        t = self.table('PaymConfig', area="Sta_Data_64", label="Payment Configuration", dump_name="paymconfig", desc="Configuration rules for payments")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('PaymType', 'integer', format=">9", initial="0", max_width=4, label="Payment Type", column_label="PType", position=3, order=20, help="Payment type")
        t.column('PaymSrc', 'character', format="x(8)", initial="", max_width=16, label="Payment Source", column_label="Source", position=4, order=30, help="Source of payment")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Date when rule becomes effective")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=6, order=50, help="Date when usage of this rule ends")
        t.column('DebitAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Debit Account", column_label="Debit", position=7, order=60, help="Account for debit posting")
        t.column('CreditAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Credit Account", column_label="Credit", position=8, order=70, help="Account for credit posting")
        t.column('Description', 'character', format="x(50)", initial="", max_width=100, label="Description", position=9, order=80, help="Description of where to rule is used")
        t.column('PaymConfig', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Configuration ID", column_label="ID", position=10, order=90, help="Configuration ID, used in links")
        t.column('TaxRules', 'logical', format="Yes/No", initial="no", max_width=1, label="Tax Rules", column_label="Tax", position=11, order=100, help="Are there rules for tax accounts")
        t.index('PaymType', [['Brand'], ['PaymType'], ['PaymSrc'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('PaymConfig', [['PaymConfig']], area="Sta_Index_2", unique=True)
        t.index('PaymSrc', [['Brand'], ['PaymSrc'], ['PaymType'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PaymConfig')
