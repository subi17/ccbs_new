from gearbox.migrations import Migration

class AddTablePaymVouch(Migration):

    database = "common"

    def up(self):
        t = self.table('PaymVouch', area="Sta_Data_256", label="Payment Voucher", dump_name="paymvouc", desc="Number series for payment vouchers")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('Voucher', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Voucher Nbr", column_label="Voucher", position=3, order=20, help="Last used voucher number")
        t.column('VoucherType', 'integer', format=">9", initial="0", max_width=4, label="Voucher Type", column_label="Type", position=4, order=30, help="Voucher type")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Date when sequence becomes effective")
        t.index('Voucher', [['Brand'], ['VoucherType'], ['FromDate', 'DESC'], ['Voucher']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('PaymVouch')
