from gearbox.migrations import Migration

class AddTableTPService(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('TPService')
        t.column('SkyTvVoucher', 'character', format="X(12)", initial="", max_width=24, label="Voucher", column_label="Voucher", position=18, order=180, help="Sky Tv Voucher")
        t.column('VoucherStatus', 'character', format="X(12)", initial="", max_width=24, label="Voucher Status", column_label="Voucher Status", position=19, order=190, help="Voucher Status Code")
        t.column('RedemFile', 'character', format="X(35)", initial="", max_width=70, label="Redemption File", column_label="Redemption File", position=20, order=200, help="Redemption File Name")
        t.column('VoucherActiveDt', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Voucher Active", column_label="Voucher Active", position=21, order=210, help="Voucher Active Date")
        t.column('VoucherExpiryDt', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Voucher Expiry", column_label="Voucher Expiry", position=22, order=220, help="Voucher Expiry Date")
        t.column('VoucherRedemDt', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Voucher Redem", column_label="Voucher Redem", position=23, order=230, help="Voucher Redem Date")
        t.column('VoucherCancelDt', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Voucher Cancel", column_label="Voucher Cancel", position=24, order=240, help="Voucher Cancel Date")
        t.index('Voucher', [['SkyTvVoucher']], area="Dyn_Index_1")
        t.index('VoucherStatus', [['VoucherStatus'], ['Operation'], ['ServType'], ['ServStatus']], area="Dyn_Index_1")
        t.alter_index('MsSeqTypeStatus', [['MsSeq'], ['Operation'], ['ServType'], ['ServStatus'], ['UpdateTS']], area="Dyn_Index_1")

    def down(self):
        t = self.alter_table('TPService')
        t.alter_index('MsSeqTypeStatus', [['MsSeq'], ['Operation'], ['ServType'], ['ServStatus']], area="Dyn_Index_1")
        t.drop_index('Voucher')
        t.drop_index('VoucherStatus')
        t.drop_column('SkyTvVoucher')
        t.drop_column('VoucherStatus')
        t.drop_column('RedemFile')
        t.drop_column('VoucherActiveDt')
        t.drop_column('VoucherExpiryDt')
        t.drop_column('VoucherRedemDt')
        t.drop_column('VoucherCancelDt')
